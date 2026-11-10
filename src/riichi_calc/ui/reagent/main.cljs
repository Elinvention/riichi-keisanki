(ns riichi-calc.ui.reagent.main
  (:require
   ["react-dom/client" :refer [createRoot]]
   [goog.dom :as gdom]
   [reagent.core :as r]
   [riichi-calc.game :as game]
   [riichi-calc.hand :as hand]
   [riichi-calc.state :as common-state]
   [riichi-calc.tile :as tile]
   [riichi-calc.ui.reagent.audio :as audio]
   [riichi-calc.ui.reagent.game :as ui-game]
   [riichi-calc.ui.reagent.history :as history]
   [riichi-calc.ui.reagent.reagentState :as state]
   [riichi-calc.ui.reagent.svg :as svg]
   [riichi-calc.ui.reagent.widget :as widget]
   [riichi-calc.ui.reagent.wizard :as wizard]
   [riichi-calc.ui.reagent.settings :as settings]))

;(enable-console-print!)

(defonce *state (r/atom (merge state/initial-state game/initial-state {:winner nil :looser nil})))

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
  (let [{:keys [keyboard-mode hand theme language]} @*state
        enabled? (partial common-state/can-input? keyboard-mode hand)]
    [:div.field.flex-center
     [widget/keyboard *state theme tile/all-34-tiles-with-redfives enabled? state/keyboard-input!]
     [widget/keyboard-mode-render *state theme]
     [widget/notation *state]
     [widget/extra-yaku *state (:extra-yaku hand) language]
     [widget/extra *state (get-in hand [:extra :dora]) (get-in hand [:extra :yaku])]]))

(defn- advance-bakaze! []
  (swap! *state (fn [st]
                  (let [wind (tile/wind-next (get-in st [:round :wind]))]
                    (-> st
                        (assoc-in [:round :wind] wind)
                        (assoc-in [:hand :bakaze] wind))))))

(defn- bakaze-button [theme]
  (let [wind (get-in @*state [:round :wind])]
    [:div.tile-button [:div "Bakaze"]
     (assoc-in (svg/tile theme (tile/wind wind)) [1 :on-click] advance-bakaze!)]))

(defn change-player-name! [player-idx name]
  (swap! *state assoc-in [:players player-idx :name] name))

(defn scoring-hand [state]
  (let [winner (js/parseInt (or (:winner state) (:dealer state) 0))]
    (-> (:hand state)
        (assoc :jikaze (game/player-wind state winner))
        (assoc :bakaze (get-in state [:round :wind])))))

(defn new-hand! []
  (let [{:keys [hand dealer round]} @*state
        bakaze (:wind round)
        jikaze (game/player-wind @*state dealer)]
    (history/save-hand! hand)
    (state/reset-initial! *state)
    (swap! *state (fn [st]
                    (-> st
                        (assoc :winner nil :looser nil)
                        (assoc-in [:hand :bakaze] bakaze)
                        (assoc-in [:hand :jikaze] jikaze))))
    (wizard/restart!)))

(defn apply-score-and-reset! [result winner-idx looser-idx]
  (let [st @*state
        game-state {:players (:players st)
                    :current-hand result}
        agari (:agari (:hand st))]
    (swap! *state assoc-in [:hand :jikaze] (game/player-wind st winner-idx))
    (case agari
      :ron (swap! *state assoc :players (:players (game/ron game-state winner-idx looser-idx)))
      :tsumo (swap! *state assoc :players (:players (game/tsumo game-state winner-idx))))
    (swap! *state assoc :dealer (game/next-dealer st winner-idx))
    (new-hand!)))

(defn tsumo-select! [winner-idx]
  (swap! *state #(-> %
                     (assoc :winner winner-idx :looser nil)
                     (assoc-in [:hand :agari] :tsumo))))

(defn ron-select! [winner-idx]
  (swap! *state #(-> %
                     (assoc :winner winner-idx :looser nil)
                     (assoc-in [:hand :agari] :ron))))

(defn ron-loser-select! [looser-idx]
  (swap! *state assoc :looser looser-idx))

(defn cancel-selection! []
  (swap! *state #(dissoc % :winner :looser)))

(defn selection-complete? [state]
  (and (some? (:winner state))
       (case (:agari (:hand state))
         :tsumo true
         :ron (some? (:looser state))
         false)))

(defn apply-selection! [result]
  (let [st @*state
        winner (:winner st)
        looser (:looser st)]
    (when (and (= :winning (:type result))
               (selection-complete? st))
      (apply-score-and-reset! result
                              (js/parseInt winner)
                              (when (some? looser) (js/parseInt looser))))))

(defonce *confirm-reset (r/atom false))

(defn reset-match! []
  (swap! *state game/reset-match)
  (state/reset-initial! *state)
  (swap! *state #(-> %
                     (assoc :winner nil :looser nil)
                     (assoc-in [:hand :bakaze] (get-in % [:round :wind]))
                     (assoc-in [:hand :jikaze] (game/player-wind % (:dealer %)))))
  (reset! *confirm-reset false))

(defn reset-match-modal []
  (when @*confirm-reset
    [:div#reset-match-modal.modal.is-active
     [:div.modal-background {:on-click #(reset! *confirm-reset false)}]
     [:div.modal-content
      [:div.box
       [:h2.title "Reset Match"]
       [:p "Reset scores, round and dealer? This cannot be undone."]
       [:div.field.is-grouped
        [:button.button.is-danger {:on-click reset-match!} "Reset"]
        [:button.button.is-light {:on-click #(reset! *confirm-reset false)} "Cancel"]]]]
     [:button.modal-close.is-large {:aria-label "close"
                                    :on-click #(reset! *confirm-reset false)}]]))

(defn match-menu []
  [:div.field.buttons
   [:button.button.is-light {:on-click settings/open!} "Settings"]
   [:button.button.is-danger.is-light {:on-click #(reset! *confirm-reset true)} "Reset Match"]])

(defn hand-menu []
  [:div.field.buttons
   [:button.button.is-info {:on-click wizard/open!} "Wizard"]
   [:button.button.is-primary {:on-click new-hand!} "New hand"]])

(defn hand-agaripai-dorahyouji [theme hand]
  [:div#hand-properties.field
   (widget/agaripai-view *state (:agaripai hand))
   (widget/dorahyouji *state hand)])

(defn hand-properties-render []
  (let [{:keys [hand theme]} @*state]
    [hand-agaripai-dorahyouji theme hand]))

(defn selection-actions [result]
  (let [st @*state
        complete? (selection-complete? st)
        winning? (= :winning (:type result))]
    [:div.field.is-grouped
     [:div.control
      [:button.button.is-primary
       {:disabled (not (and complete? winning?))
        :on-click #(apply-selection! result)}
       "Apply Score Change"]]
     [:div.control
      [:button.button.is-light
       {:on-click cancel-selection!}
       "Cancel"]]]))

(defn result-win [lang {:keys [score] :as result}]
  [:<>
   [widget/button-play-results-speech lang result]
   [widget/results-table-render lang result]
   [widget/score-table-render score]
   [selection-actions result]])


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
  (let [{:keys [theme language] :as st} @*state
        hand (scoring-hand st)
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
   [match-menu]
   [bakaze-button (:theme @*state)]
   [ui-game/player-scores
    (:players @*state)
    (:dealer @*state)
    {:winner (:winner @*state)
     :looser (:looser @*state)
     :agari (get-in @*state [:hand :agari])
     :on-name-change change-player-name!
     :on-tsumo tsumo-select!
     :on-ron ron-select!
     :on-ron-loser ron-loser-select!}]
   [hand-menu]
   [hand-properties-render]
   [widget/hand-render (:theme @*state) (:hand @*state) (partial state/remove-from-hand! *state)]
   [keyboard-render]
   [settings/render *state]
   [wizard/render *state]
   [reset-match-modal]])


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

(defn- register-service-worker!
  []
  (when (exists? js/navigator.serviceWorker)
    (let [had-controller? (some? (.-controller js/navigator.serviceWorker))
          reloaded (atom false)]
      (.addEventListener js/navigator.serviceWorker "controllerchange"
                         (fn [_]
                           (when (and had-controller? (not @reloaded))
                             (reset! reloaded true)
                             (.reload js/location))))
      (-> (.register js/navigator.serviceWorker "sw.js" #js {:updateViaCache "none"})
          (.catch (fn [err] (js/console.warn "Service worker registration failed:" err)))))))

(defn init
  []
  (settings/load-settings! *state)
  (audio/play-tile-down-sfx)
  (history/init!)
  (register-service-worker!)
  (.render root-app (r/as-element [root-render])))

#_{:clojure-lsp/ignore [:clojure-lsp/unused-public-var]}
(defn ^:dev/after-load re-render
  []
  ;; The `:dev/after-load` metadata causes this function to be called
  ;; after shadow-cljs hot-reloads code.
  ;; This function is called implicitly by its annotation.
  (init))
