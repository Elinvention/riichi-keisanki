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
  {:open false :status :idle :image nil :original-image nil
   :detections [] :error nil :progress nil :inference-ms nil})

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
  "Reads an image File into a base64 data URL at its original resolution."
  [file callback]
  (let [reader (js/FileReader.)]
    (set! (.-onload reader) (fn [_] (callback (.-result reader))))
    (.readAsDataURL reader file)))

(defn- handle-file! [event]
  (when-let [file (some-> event .-target .-files (aget 0))]
    (file->image-data file
                      #(swap! *scanner assoc
                              :image %
                              :original-image %
                              :status :idle :error nil :progress nil
                              :inference-ms nil))))

(defn- default-square-crop [w h]
  (let [size (min w h)]
    {:x (/ (- w size) 2.0)
     :y (/ (- h size) 2.0)
     :size size}))

(defn- crop-image! [image crop]
  (-> (load-image image)
      (.then
       (fn [img]
         (let [{:keys [x y size]} crop
               side (js/Math.round size)
               canvas (js/document.createElement "canvas")]
           (set! (.-width canvas) side)
           (set! (.-height canvas) side)
           (let [ctx (.getContext canvas "2d")]
             (.drawImage ctx img (js/Math.round x) (js/Math.round y)
                         side side 0 0 side side))
           (swap! *scanner assoc
                  :image (.toDataURL canvas "image/jpeg" 0.9)
                  :status :idle
                  :detections []
                  :error nil
                  :inference-ms nil))))
      (.catch
       (fn [err]
         (swap! *scanner assoc
                :status :error
                :error (str "Could not crop photo: " err))))))

(defn- restore-image! []
  (when-let [original (:original-image @*scanner)]
    (swap! *scanner assoc
           :image original
           :status :idle
           :detections []
           :error nil
           :inference-ms nil)))

(defn- event->image-point [e rect w h]
  {:x (* (- (.-clientX e) (.-left rect)) (/ w (.-width rect)))
   :y (* (- (.-clientY e) (.-top rect)) (/ h (.-height rect)))})

(defn- crop-pointer-down! [*drag *crop {:keys [w h]} e]
  (when (and (pos? (or w 0)) (pos? (or h 0)) @*crop)
    (let [target (.-target e)
          handle (some-> target (.getAttribute "data-crop-handle") not-empty keyword)
          in-box? (or handle (some-> target (.closest "[data-crop-box]")))
          current (.-currentTarget e)
          rect (.getBoundingClientRect current)
          point (event->image-point e rect w h)]
      (when in-box?
        (.preventDefault e)
        (.setPointerCapture current (.-pointerId e))
        (reset! *drag {:mode (if handle :resize :move)
                       :handle handle
                       :rect rect
                       :w w
                       :h h
                       :crop @*crop
                       :start-x (:x point)
                       :start-y (:y point)})))))

(defn- update-crop-drag! [*crop *drag e]
  (when-let [{:keys [mode handle rect w h crop start-x start-y]} @*drag]
    (let [{px :x py :y} (event->image-point e rect w h)
          dx (- px start-x)
          dy (- py start-y)
          {:keys [x y size]} crop
          min-size (min 32.0 (min w h))
          clamp (fn [v lo hi] (max lo (min hi v)))
          new-crop
          (case mode
            :move
            {:x (clamp (+ x dx) 0.0 (- w size))
             :y (clamp (+ y dy) 0.0 (- h size))
             :size size}

            :resize
            (case handle
              :se
              (let [ax x
                    ay y
                    max-size (min (- w ax) (- h ay))
                    s (min max-size (max min-size (max (+ size dx) (+ size dy))))]
                {:x ax :y ay :size s})

              :nw
              (let [ax (+ x size)
                    ay (+ y size)
                    max-size (min ax ay)
                    s (min max-size (max min-size (max (- size dx) (- size dy))))]
                {:x (- ax s) :y (- ay s) :size s})

              :ne
              (let [ax x
                    ay (+ y size)
                    max-size (min (- w ax) ay)
                    s (min max-size (max min-size (max (+ size dx) (- size dy))))]
                {:x ax :y (- ay s) :size s})

              :sw
              (let [ax (+ x size)
                    ay y
                    max-size (min ax (- h ay))
                    s (min max-size (max min-size (max (- size dx) (+ size dy))))]
                {:x (- ax s) :y ay :size s})))]
      (reset! *crop new-crop))))

(defn- crop-box-style [{:keys [x y size]} w h]
  {:position "absolute"
   :left (str (* 100.0 (/ x w)) "%")
   :top (str (* 100.0 (/ y h)) "%")
   :width (str (* 100.0 (/ size w)) "%")
   :height (str (* 100.0 (/ size h)) "%")
   :box-sizing "border-box"
   :border "2px solid rgba(255, 255, 255, 0.95)"
   :box-shadow "0 0 0 1px rgba(0, 0, 0, 0.45), 0 0 0 9999px rgba(0, 0, 0, 0.45)"
   :cursor "move"
   :touch-action "none"})

(defn- crop-handle-style [corner]
  (merge
   {:position "absolute"
    :width "14px"
    :height "14px"
    :box-sizing "border-box"
    :background "rgba(255, 255, 255, 0.95)"
    :border "1px solid rgba(0, 0, 0, 0.6)"
    :touch-action "none"}
   (case corner
     :nw {:left "0" :top "0" :cursor "nwse-resize"}
     :ne {:right "0" :top "0" :cursor "nesw-resize"}
     :sw {:left "0" :bottom "0" :cursor "nesw-resize"}
     :se {:right "0" :bottom "0" :cursor "nwse-resize"})))

(defn- photo-preview [image original-image status]
  (let [*dims (r/atom nil)
        *crop (r/atom nil)
        *drag (r/atom nil)]
    (fn [image original-image status]
      (let [dims (when (= image (:src @*dims)) @*dims)
            {:keys [w h]} dims
            crop @*crop
            ready? (and w h crop)
            cropped? (and original-image (not= image original-image))
            scanning? (= status :scanning)]
        [:div.mt-4
         [:div {:style {:position "relative"
                        :display "inline-block"
                        :max-width "100%"
                        :overflow "hidden"
                        :user-select "none"}
               :on-pointer-down (fn [e]
                                  (when ready?
                                    (crop-pointer-down! *drag *crop dims e)))
               :on-pointer-move (fn [e]
                                  (when @*drag
                                    (update-crop-drag! *crop *drag e)))
               :on-pointer-up (fn [_] (reset! *drag nil))
               :on-pointer-cancel (fn [_] (reset! *drag nil))}
          [:img {:src image
                 :alt "hand preview"
                 :draggable false
                 :on-drag-start (fn [e] (.preventDefault e))
                 :on-load (fn [e]
                            (let [img (.-target e)
                                  w (.-naturalWidth img)
                                  h (.-naturalHeight img)]
                              (reset! *dims {:src image :w w :h h})
                              (reset! *crop (default-square-crop w h))
                              (reset! *drag nil)))
                 :style (cond-> {:max-height "240px"
                                 :max-width "100%"
                                 :width "auto"
                                 :height "auto"
                                 :display "block"
                                 :object-fit "contain"}
                          (and w h) (assoc :aspect-ratio (str w " / " h)))}]
          (when ready?
            [:div {:data-crop-box "true"
                   :style (crop-box-style crop w h)}
             (for [corner [:nw :ne :sw :se]]
               ^{:key (name corner)}
               [:div {:data-crop-handle (name corner)
                      :style (crop-handle-style corner)}])])]
         [:p.is-size-7.has-text-grey.mt-2
          (if ready?
            (str "Photo " w "×" h " px · crop "
                 (js/Math.round (:size crop)) "×"
                 (js/Math.round (:size crop))
                 " px. Drag the square to move it; drag a corner to resize.")
            "Loading preview…")]
         [:div.field.is-grouped.mt-2
          [:button.button.is-light
           {:disabled (or (not ready?) scanning?)
            :on-click (fn []
                        (when (and ready? (not scanning?))
                          (crop-image! image crop)))}
           "Crop to square"]
          [:button.button.is-light
           {:disabled (or (not cropped?) scanning?)
            :on-click (fn []
                        (when (and cropped? (not scanning?))
                          (restore-image!)))}
           "Restore"]]]))))

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
  (let [{:keys [status image original-image detections error progress inference-ms]}
        @*scanner]
    [:<>
     [photo-input image]
     (when-not (s/blank? image)
       [photo-preview image original-image status])
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
