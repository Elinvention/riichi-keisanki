(ns riichi-calc.ui.reagent.main
  (:require
   ["react-dom/client" :refer [createRoot]] 
   [clojure.string :as s]
   [goog.dom :as gdom]
   [reagent.core :as r]
   [riichi-calc.hand :as hand]
   [riichi-calc.state :as common-state]
   [riichi-calc.tile :as tile]
   [riichi-calc.ui.reagent.audio :as audio]
   [riichi-calc.ui.reagent.history :as history]
   [riichi-calc.ui.reagent.wizard :as wizard]
   [riichi-calc.ui.reagent.widget :as widget]
   [riichi-calc.ui.reagent.settings :as settings]
   [riichi-calc.ui.reagent.reagentState :as state]
   [riichi-calc.ui.reagent.svg :as svg]))

;(enable-console-print!)

(defonce *state (r/atom state/initial-state))

(goog-define VERSION "0.0.0")
(goog-define BUILD-SHA "UNKNOWN")
(goog-define BUILD-DATE "UNKNOWN")

(defn- glossary-component []
  [:section.section
   [:h2.title.is-2 "Glossary"]
   [:ul
    [:li "Bakaze 場風 turn wind"]
    [:li "Jikaze 自風 (own) seat wind"]
    [:li "Dorahyoji ドラ表示 dora indicator"]
    [:li "Agari 和がり generic call for winning a hand"]
    [:li "Agaripai 和了り牌 winning tile"]
    [:li "Ron 栄 win by deal in"]
    [:li "Tsumo 自摸 win by self draw"]
    [:li "An 暗 \"dark\" concealed tiles"]
    [:li "Pon ポン call for open triplet"]
    [:li "Chii チイ call for open straight"]
    [:li "Kan 槓 call for open quad"]
    [:li "Ankan 暗槓 call for concealed kan"]
    [:li "Akadora 赤ドラ red fives"]]
   [:p "For more information visit the " [:a {:href "https://riichi.wiki/"} "Riichi Wiki"] "."]])

(defn- footer-component []
  [:footer
   [:p (str "riichi-keisanki version " VERSION " (" BUILD-SHA " - " BUILD-DATE ") "
            "Copyright © 2022 Elia Argentieri. ")]
   [:p "Source code on "
    [:a {:href "https://github.com/Elinvention/riichi-keisanki" :target "_blank"} "Github"]
    " and on "
    [:a {:href "https://code.elinvention.ovh/Elinvention/riichi-keisanki" :target "_blank"} "my gitea instance"] "."]
   [:p "Mahjong tiles by " [:a {:href "https://github.com/FluffyStuff/riichi-mahjong-tiles/" :target "_blank"} "FluffyStuff"] "."]])

(defn keyboard-render []
  (let [{:keys [keyboard-mode hand theme]} @*state 
        enabled? (partial common-state/can-input? keyboard-mode hand)]
    [:div.field.flex-center
     [widget/keyboard *state theme tile/all-34-tiles-with-redfives enabled? state/keyboard-input!]
     [widget/keyboard-mode-render *state theme]
     [widget/notation *state]]))

(defn- advance-wind [wind]
  (swap! *state update-in [:hand wind] tile/wind-next))

(defn- wind-button [wind kind theme]
  [:div.tile-button [:div (s/capitalize (name kind))]
   (assoc-in (svg/tile theme wind) [1 :on-click] #(advance-wind kind))])

(defn new-hand! []
  (let [{:keys [hand]} @*state]
    (history/save-hand! hand)
    (state/reset-initial! *state)
    (wizard/restart!)))

(defn buttons []
  [:div.field.buttons
   [:button.button.is-info {:on-click wizard/open!} "Wizard"]
   [:button.button.is-primary {:on-click new-hand!} "New hand"]
   [:button.button.is-light {:on-click settings/open!} "Settings"]])

(defn hand-wind-agaripai-dorahyouji [theme hand]
  [:div#hand-properties.field
   (wind-button (tile/wind (:bakaze hand)) :bakaze theme)
   (wind-button (tile/wind (:jikaze hand)) :jikaze theme)
   (widget/agaripai-view *state (:agaripai hand))
   (widget/dorahyouji *state hand)])

(defn hand-properties-render []
  (let [{:keys [hand theme language]} @*state]
    [:<>
     [hand-wind-agaripai-dorahyouji theme hand]
     [widget/agari *state (:agari hand)]
     [widget/extra-yaku *state (:extra-yaku hand) language]
     [widget/extra *state (get-in hand [:extra :dora]) (get-in hand [:extra :yaku])]]))

(defn result-win [lang {:keys [score] :as result}]
  [:<>
   [widget/button-play-results-speech lang result]
   [widget/results-table-render lang result]
   [widget/score-table-render score]])

(defn ukeire-tile [theme tile]
  [assoc-in (svg/tile theme tile) [1 :on-click]
   #(swap! *state common-state/add-agaripai tile)])

(defn result-tenpai [theme {:keys [ukeire summary]}]
  [:<>
   [:p summary]
   [:div.tile-row
    (for [tile (tile/sort-tiles ukeire)]
      ^{:key (str "tenpai" theme (tile/tile-name tile))}
      [ukeire-tile theme tile])]])

(defn agaripai-tile [theme tile]
  [assoc-in (svg/tile theme tile) [1 :on-click]
   #(swap! *state common-state/set-agaripai tile)])

(defn result-agaripai [{:keys [an]} theme {:keys [summary]}]
  [:<>
   [:p summary]
   [:div.tile-row
    (for [tile (dedupe (tile/sort-tiles an))]
      ^{:key (str "agaripai" theme (tile/tile-name tile))}
      [agaripai-tile theme tile])]])

(defn results-render []
  (let [{:keys [hand theme language]} @*state
        {:keys [summary] :as res} (hand/results hand language)]
    (case (:type res)
      (:incomplete :invalid :no-yaku) [:p summary]
      :agaripai (result-agaripai hand theme res)
      :tenpai (result-tenpai theme res)
      :winning (result-win language res))))

(defn restore-hand! [hand]
  (swap! *state assoc :hand hand))

(defn app-render []
  [:<>
   [buttons]
   [widget/hand-render (:theme @*state) (:hand @*state) (partial state/remove-from-hand! *state)]
   [keyboard-render]
   [hand-properties-render]
   [settings/render *state]
   [wizard/render *state]])


(defn root-render []
  [:<>
   [:section.hero
    [:div#interactive
     [app-render]]]
   [:section.section
    [:h2.title.is-2 "Results"]
    [:div#results
     [results-render]]]
   [:section.section
    [:h2.title.is-2 "History"]
    [:div#history
     [history/render (:theme @*state) widget/hand-render restore-hand!]]]
   [glossary-component]
   [footer-component]])

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
  ;; The `:dev/after-load` metadata causes this function to be called
  ;; after shadow-cljs hot-reloads code.
  ;; This function is called implicitly by its annotation.
  (init))
