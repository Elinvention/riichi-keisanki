(ns riichi-calc.ui.reagent.main
  (:require
   ["react-dom/client" :refer [createRoot]]
   [goog.dom :as gdom]
   [reagent.core :as r]
   [riichi-calc.hand :as hand]
   [riichi-calc.ui.reagent.audio :as audio]
   [riichi-calc.ui.reagent.history :as history]
   [riichi-calc.ui.reagent.reagentState :as state]
   [riichi-calc.ui.reagent.settings :as settings]
   [riichi-calc.ui.reagent.widget.base :as widget]
   [riichi-calc.ui.reagent.widget.keyboard :as keyboard]
   [riichi-calc.ui.reagent.widget.properties :as properties]
   [riichi-calc.ui.reagent.widget.results :as results]
   [riichi-calc.ui.reagent.wizard :as wizard]))

(defonce *state (r/atom state/initial-state))

(defn- new-hand! [*state]
  (history/save-hand! (:hand @*state))
  (state/reset-initial! *state)
  (wizard/restart!))

(defn- buttons [*state]
  [:div.field.buttons
   [:button.button.is-info {:on-click wizard/open!} "Wizard"]
   [:button.button.is-primary {:on-click #(new-hand! *state)} "New hand"]
   [:button.button.is-light {:on-click settings/open!} "Settings"]])

(defn restore-hand! [hand]
  (swap! *state assoc :hand hand))

(defn app-render []
  (let [{:keys [theme language]} @*state
        hand-cursor (r/cursor *state [:hand])]
    [:<>
     [buttons *state]
     [widget/hand-render theme @hand-cursor (fn [path index] (swap! hand-cursor hand/remove-from-hand path index))]
     [keyboard/main-keyboard *state]
     [properties/hand-properties theme language hand-cursor]
     [settings/render *state]
     [wizard/render *state]]))

(defn root-render []
  [:<>
   [:section.hero
    [:div#interactive
     [app-render]]]
   [:section.section
    [:h2.title.is-2 "Results"]
    [:div#results
     [results/render *state]]]
   [:section.section
    [:h2.title.is-2 "History"]
    [:div#history
     [history/render (:theme @*state) widget/hand-render restore-hand!]]]])


(defonce root-app (createRoot (gdom/getElement "app")))

(defn init
  []
  (settings/load-settings! *state)
  (audio/play-tile-down-sfx)
  (history/init!)
  (.render root-app (r/as-element [root-render])))

#_{:clojure-lsp/ignore [:clojure-lsp/unused-public-var]}
(defn ^:dev/after-load re-render
  []
  (init))
