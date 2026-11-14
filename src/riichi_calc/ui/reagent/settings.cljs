(ns riichi-calc.ui.reagent.settings
  (:require
   [reagent.core :as r]
   [cljs.reader :as reader]
   [riichi-calc.ui.reagent.widget :as widget]
   [riichi-calc.ui.reagent.onnx :as onnx]
   [riichi-calc.vision.yolo :as yolo]))

(defonce *settings (r/atom {:open false}))

(def ^:private settings-serialization-version 3)

(defn open! []
  (onnx/refresh-models!)
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
            (when-let [model (:scan-model parsed-settings)]
              (swap! *state assoc :scan-model model))
            (when-let [confidence (:scan-confidence parsed-settings)]
              (swap! *state assoc :scan-confidence confidence))
            (when-let [iou (:scan-iou parsed-settings)]
              (swap! *state assoc :scan-iou iou)))
          (js/console.warn "Stored settings version is missing or newer than expected. Ignoring."))))))

(defn save-settings! [*state]
  (try
    (let [settings {:version settings-serialization-version
                    :theme (:theme @*state)
                    :language (:language @*state)
                    :scan-model (:scan-model @*state)
                    :scan-confidence (:scan-confidence @*state)
                    :scan-iou (:scan-iou @*state)}
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

(defn number-input [*state k default min max step]
  [:div.control
   [:input.input {:type :number :min min :max max :step step
                  :value (or (get @*state k) default)
                  :on-change (fn [e]
                               (let [v (js/parseFloat (.. e -target -value))]
                                 (swap! *state assoc k (when-not (js/isNaN v) v)))
                               (save-settings! *state))}]])

(defn- model-label [{:keys [key size]}]
  (if (number? size)
    (str key " (" (.toFixed (/ size 1048576.0) 1) " MB)")
    key))

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
  [:div.field
   [:label.label "Model"]
   [model-selector *state]
   [:p.help "On-device YOLO ONNX model, run in the browser with "
    "onnxruntime-web. All bundled models are available."]
   [:label.label.mt-2 "IoU threshold (NMS)"]
   [number-input *state :scan-iou yolo/default-iou 0 1 0.05]
   [:label.label.mt-2 "Detection confidence threshold"]
   [number-input *state :scan-confidence yolo/default-confidence 0 1 0.05]])

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
