(ns riichi-calc.ui.reagent.scanner
  "Modal that captures a photo from the device camera, runs an on-device
   mahjong tile detection model (YOLO via onnxruntime-web) and populates the
   concealed hand."
  (:require
   [clojure.string :as s]
   [reagent.core :as r]
   [riichi-calc.tile :as tile]
   [riichi-calc.vision :as vision]
   [riichi-calc.vision.yolo :as yolo]
   [riichi-calc.ui.reagent.onnx :as onnx]
   [riichi-calc.ui.reagent.svg :as svg]))

(def initial-scanner
  {:open false :status :idle :image nil :detections [] :error nil
   :progress nil :inference-ms nil})

(defonce *scanner (r/atom initial-scanner))

(defn- preload-model!
  "Starts downloading the selected on-device model as soon as the scanner is
   opened, so it is ready by the time the user runs a detection."
  [*state]
  (onnx/preload! (onnx/model-url (get @*state :scan-model))
                 (fn [loaded total]
                   (swap! *scanner assoc :progress
                          (when (or (not (pos? total)) (< loaded total))
                            {:loaded loaded :total total})))))

(defn open! [*state]
  (reset! *scanner (assoc initial-scanner :open true))
  (preload-model! *state))

(defn close! []
  (reset! *scanner initial-scanner))

(defn- load-image [data-url]
  (js/Promise.
   (fn [resolve reject]
     (let [img (js/Image.)]
       (set! (.-onload img) (fn [_] (resolve img)))
       (set! (.-onerror img) (fn [_] (reject (js/Error. "Failed to load image"))))
       (set! (.-src img) data-url)))))

(defn- scan-confidence [*state]
  (js/parseFloat (or (get @*state :scan-confidence)
                     yolo/default-confidence)))

(defn- set-results! [detections]
  (swap! *scanner assoc
         :detections detections
         :status (if (empty? detections) :empty :results)))

(defn- onnx-scan! [*state]
  (let [image (:image @*scanner)
        model-path (onnx/model-url (get @*state :scan-model))
        confidence (scan-confidence *state)
        iou (js/parseFloat (or (get @*state :scan-iou) yolo/default-iou))]
    (swap! *scanner assoc :status :scanning :error nil :progress nil
           :inference-ms nil :detections [])
    (-> (load-image image)
        (.then (fn [img]
                 (onnx/run-detection! img {:model-path model-path
                                           :confidence confidence
                                           :iou iou
                                           :on-progress
                                           (fn [loaded total]
                                             (swap! *scanner assoc
                                                    :progress {:loaded loaded
                                                               :total total}))})))
        (.then (fn [{:keys [detections inference-ms]}]
                 (swap! *scanner assoc :inference-ms inference-ms)
                 (set-results! (vision/parse-detections detections confidence))))
        (.catch (fn [err]
                  (swap! *scanner assoc
                         :status :error
                         :error (str "On-device model failed: " err)))))))

(defn- scan! [*state]
  (if (s/blank? (:image @*scanner))
    (swap! *scanner assoc :status :error :error "Take or choose a photo first.")
    (onnx-scan! *state)))

(defn- file->image-data
  "Reads an image File, downscales it so its longest side is at most `max-dim`
   pixels and calls `callback` with a base64 JPEG data URL."
  [file max-dim callback]
  (let [reader (js/FileReader.)]
    (set! (.-onload reader)
          (fn [_]
            (let [img (js/Image.)]
              (set! (.-onload img)
                    (fn [_]
                      (let [w (.-width img)
                            h (.-height img)
                            scale (min 1.0 (/ max-dim (js/Math.max w h)))
                            nw (js/Math.round (* w scale))
                            nh (js/Math.round (* h scale))
                            canvas (js/document.createElement "canvas")]
                        (set! (.-width canvas) nw)
                        (set! (.-height canvas) nh)
                        (let [ctx (.getContext canvas "2d")]
                          (.drawImage ctx img 0 0 nw nh)
                          (callback (.toDataURL canvas "image/jpeg" 0.85))))))
              (set! (.-src img) (.-result reader)))))
    (.readAsDataURL reader file)))

(defn- handle-file! [event]
  (when-let [file (some-> event .-target .-files (aget 0))]
    (file->image-data file 640
                      #(swap! *scanner assoc :image %
                              :status :idle :error nil :progress nil
                              :inference-ms nil))))

(defn- apply! [*state]
  (let [tiles (mapv :tile (:detections @*scanner))]
    (swap! *state #(-> %
                       (assoc-in [:hand :an] (tile/sort-tiles tiles))
                       (assoc :keyboard-mode :an)))
    (close!)))

(defn- discard! [index]
  (swap! *scanner update :detections
         (fn [dets] (vec (concat (subvec dets 0 index) (subvec dets (inc index)))))))

(defn- detected-tiles [*state detections]
  (let [theme (:theme @*state)]
    [:div.tile-row {:style {:flex-wrap "wrap"}}
     (doall
      (for [[i {:keys [tile confidence class]}] (map-indexed vector detections)]
        ^{:key (str "detected" i class confidence)}
        [:div.tile-button {:title (str class " " (js/Math.round (* 100 confidence)) "%")
                           :on-click #(discard! i)}
         (svg/tile theme tile)]))]))

(defn- photo-input [image]
  [:div.file.has-name.is-fullwidth
   [:label.file-label
    [:input.file-input {:type :file
                        :accept "image/*"
                        :capture "environment"
                        :on-change handle-file!}]
    [:span.file-cta
     [:span.file-icon [:i.fas.fa-upload]]
     [:span.file-label "Choose photo"]]
    [:span.file-name (if (s/blank? image) "No photo chosen" "Photo ready")]]])

(defn- format-mb [bytes]
  (str (.toFixed (/ bytes 1048576.0) 1) " MB"))

(defn- download-progress
  "Compact download indicator shown while the model is being fetched."
  [{:keys [loaded total]}]
  (when (and total (pos? total))
    (let [pct (min 100 (js/Math.round (* 100 (/ loaded total))))]
      [:<>
       [:p.is-size-7.has-text-grey
        (str "Downloading model " pct "% (" (format-mb loaded)
             " / " (format-mb total) ")")]
       [:progress.progress {:value (min loaded total) :max total}]])))

(defn- scanning-view [progress]
  (let [{:keys [loaded total]} progress]
    [:<>
     [:span.loader.mx-auto.mb-3]
     (if (and total (pos? total))
       (let [pct (min 100 (js/Math.round (* 100 (/ loaded total))))]
         [:<>
          [:p (if (< loaded total) "Downloading model…" "Preparing model…")]
          [:progress.progress {:value (min loaded total) :max total}]
          [:p.is-size-7.has-text-grey
           (str pct "% (" (format-mb loaded) " / " (format-mb total) ")")]])
       [:<>
        [:p "Running inference…"]
        [:p.is-size-7.has-text-grey
         "The first scan downloads and prepares the model."]])]))

(defn- status-content [*state]
  (let [{:keys [status image detections error progress inference-ms]} @*scanner]
    [:<>
     [photo-input image]
     (when-not (s/blank? image)
       [:figure.image.mt-4
        [:img {:src image :alt "hand preview"
               :style {:max-height "240px" :display "block"}}]])
     (case status
       :scanning
       [:div.has-text-centered.mt-4
        [scanning-view progress]]

       :results
       [:div.mt-4
        [:p (str "Detected " (count detections) " tiles"
                 (when inference-ms
                   (str " in " (js/Math.round inference-ms) " ms"))
                 ". Tap a tile to discard it.")]
        [detected-tiles *state detections]
        [:div.field.is-grouped.mt-4
         [:button.button.is-primary {:on-click #(apply! *state)} "Apply to hand"]
         [:button.button.is-light {:on-click #(scan! *state)} "Detect again"]]]

       :empty
       [:div.mt-4
        [:p "No tiles detected. Try a clearer, closer photo."]
        [:div.field.is-grouped
         [:button.button.is-light {:on-click #(scan! *state)} "Detect again"]]]

       :error
       [:div.mt-4
        [:p.has-text-danger error]
        [:div.field.is-grouped
         [:button.button.is-light {:on-click #(scan! *state)} "Retry"]]]

       ;; :idle
       [:div.mt-4
        [download-progress progress]
        [:div.field.is-grouped
         [:button.button.is-primary
          {:disabled (s/blank? image)
           :on-click #(scan! *state)}
          "Detect tiles"]]])]))

(defn render [*state]
  (when (:open @*scanner)
    [:div#scanner-modal.modal.is-active
     [:div.modal-background {:on-click close!}]
     [:div.modal-content
      [:div.box
       [:h2.title "Scan hand from photo"]
       [:p.is-size-7.has-text-grey
        "Runs YOLO on-device. Calls (pon/chii/kan) must be set manually "
        "afterwards."]
       [:hr]
       [status-content *state]]]
     [:button.modal-close.is-large {:aria-label "close"
                                    :on-click close!}]]))
