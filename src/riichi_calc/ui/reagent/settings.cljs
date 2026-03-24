(ns riichi-calc.ui.reagent.settings
  (:require
   [reagent.core :as r]
   [riichi-calc.ui.reagent.widget :as widget]))

(defonce *settings (r/atom {:open false}))

(defn open! []
  (swap! *settings assoc :open true))

(defn close! []
  (swap! *settings assoc :open false))

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
          [theme-selector theme #(swap! *state assoc :theme %)]
          [language-selector language #(swap! *state assoc :language %)]]]]
       [:button.modal-close.is-large {:aria-label "close"
                                      :on-click close!}]])))
