(ns riichi-calc.ui.reagent.settings
  (:require
   [reagent.core :as r]
   [cljs.reader :as reader]
   [riichi-calc.ui.reagent.widget :as widget]))

(defonce *settings (r/atom {:open false}))

(def ^:private settings-serialization-version 1)

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
              (swap! *state assoc :language language)))
          (js/console.warn "Stored settings version is missing or newer than expected. Ignoring."))))))

(defn save-settings! [*state]
  (try
    (let [settings {:version settings-serialization-version
                    :theme (:theme @*state)
                    :language (:language @*state)}
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
          [language-selector language #(change-language! *state %)]]]]
       [:button.modal-close.is-large {:aria-label "close"
                                      :on-click close!}]])))
