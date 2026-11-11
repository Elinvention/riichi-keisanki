(ns riichi-calc.ui.reagent.settings
  (:require
   [reagent.core :as r]
   [cljs.reader :as reader]
   [riichi-calc.ui.reagent.widget :as widget]
   [riichi-calc.ui.reagent.onnx :as onnx]
   [riichi-calc.vision.yolo :as yolo]))

(defonce *settings (r/atom {:open false}))

(def ^:private settings-serialization-version 2)

(defn open! []
  (swap! *settings assoc :open true))

(defn close! []
  (swap! *settings assoc :open false))

(defn ^:private parse-settings-string [settings-str]
  (try
    (reader/read-string settings-str)
    (catch :default e
      (js/console.error "Error parsing settings from local storage:" e)
      nil)))

(defn load-settings! [*state]
  (when-let [stored-settings-str (js/window.localStorage.getItem "settings")]
    (when-let [parsed-settings (parse-settings-string stored-settings-str)]
      (let [version (:version parsed-settings)]
        (if (and version (<= version settings-serialization-version))
          (do
            (when-let [theme (:theme parsed-settings)]
              (swap! *state assoc :theme theme))
            (when-let [language (:language parsed-settings)]
              (swap! *state assoc :language language))
            (when-let [backend (:scan-backend parsed-settings)]
              (swap! *state assoc :scan-backend backend))
            (when-let [model (:scan-model parsed-settings)]
              (swap! *state assoc :scan-model model))
            (when-let [confidence (:scan-confidence parsed-settings)]
              (swap! *state assoc :scan-confidence confidence))
            (when-let [iou (:scan-iou parsed-settings)]
              (swap! *state assoc :scan-iou iou))
            (when-let [api-key (:roboflow-key parsed-settings)]
              (swap! *state assoc :roboflow-key api-key)))
          (js/console.warn "Stored settings version is missing or newer than expected. Ignoring."))))))

(defn save-settings! [*state]
  (try
    (let [settings {:version settings-serialization-version
                    :theme (:theme @*state)
                    :language (:language @*state)
                    :scan-backend (:scan-backend @*state)
                    :scan-model (:scan-model @*state)
                    :scan-confidence (:scan-confidence @*state)
                    :scan-iou (:scan-iou @*state)
                    :roboflow-key (:roboflow-key @*state)}
          settings-str (pr-str settings)]
      (js/window.localStorage.setItem "settings" settings-str))
    (catch :default e
      (js/console.error "Failed to save settings to localStorage:" e))))

(defn change-theme! [*state theme]
  (swap! *state assoc :theme theme)
  (save-settings! *state))

(defn change-language! [*state language]
  (swap! *state assoc :language language)
  (save-settings! *state))

(defn theme-selector [theme on-change]
  [:fieldset.field [:legend "Theme"]
   [widget/radio-group "theme" [:regular :black] theme on-change]])

(defn language-selector [language on-change]
  [:fieldset.field [:legend "Yaku Names Language"]
   [widget/radio-group "language" [:ja :romaji :it :en] language on-change]])

(defn scan-backend-selector [*state backend]
  [:fieldset.field [:legend "Hand scanning backend"]
   [widget/radio-group "scan-backend" [:onnx :hosted] backend
    (fn [b]
      (swap! *state assoc :scan-backend b)
      (save-settings! *state))]])

(defn text-input [*state k placeholder]
  [:div.control
   [:input.input {:type :text
                  :placeholder placeholder
                  :value (or (get @*state k) "")
                  :on-change (fn [e]
                               (swap! *state assoc k (.. e -target -value))
                               (save-settings! *state))}]])

(defn number-input [*state k default min max step]
  [:div.control
   [:input.input {:type :number :min min :max max :step step
                  :value (or (get @*state k) default)
                  :on-change (fn [e]
                               (let [v (js/parseFloat (.. e -target -value))]
                                 (swap! *state assoc k (when-not (js/isNaN v) v)))
                               (save-settings! *state))}]])

(defn- model-label [{:keys [name size]}]
  (if (number? size)
    (str name " (" (.toFixed (/ size 1048576.0) 1) " MB)")
    name))

(defn model-selector [*state]
  (let [available (onnx/models)
        current (or (:scan-model @*state) (:key (first available)))]
    [:div.control
     [:div.select.is-fullwidth
      [:select {:value current
                :on-change (fn [e]
                             (swap! *state assoc :scan-model (.. e -target -value))
                             (save-settings! *state))}
       (for [model available]
         ^{:key (:key model)} [:option {:value (:key model)} (model-label model)])]]]))

(defn scan-settings [*state]
  (let [backend (if (contains? #{:onnx :hosted} (:scan-backend @*state))
                  (:scan-backend @*state)
                  :onnx)]
    [:div.field
     [scan-backend-selector *state backend]
     (case backend
       :onnx
       [:<>
        [:label.label "Model"]
        [model-selector *state]
        [:p.help "On-device YOLOv11 ONNX model, run in the browser with "
         "onnxruntime-web. All bundled models are available."]
        [:label.label.mt-2 "IoU threshold (NMS)"]
        [number-input *state :scan-iou yolo/default-iou 0 1 0.05]]
       :hosted
       [:<>
        [:label.label "Private API key (hosted)"]
        [text-input *state :roboflow-key "private_api_key"]
        [:p.help "A private API key from app.roboflow.com/settings/api. Stored only "
         "in this browser."]])
     [:label.label.mt-2 "Detection confidence threshold"]
     [number-input *state :scan-confidence yolo/default-confidence 0 1 0.05]]))

(defn render [*state]
  (let [{:keys [theme language]} @*state]
    (when (:open @*settings)
      [:div#settings-modal.modal.is-active
       [:div.modal-background {:on-click close!}]
       [:div.modal-content
        [:div.box
         [:h2.title "Settings"]
         [:div.field
          [theme-selector theme #(change-theme! *state %)]
          [language-selector language #(change-language! *state %)]]
         [scan-settings *state]]]
       [:button.modal-close.is-large {:aria-label "close"
                                      :on-click close!}]])))
