(ns riichi-calc.ui.reagent.main
  (:require
   ["react-dom/client" :refer [createRoot]] 
   [clojure.string :as s]
   [goog.dom :as gdom]
   [reagent.core :as r]
   [riichi-calc.group :as group]
   [riichi-calc.hand :as hand]
   [riichi-calc.state :as common-state]
   [riichi-calc.tile :as tile]
   [riichi-calc.yakudb :refer [yakudb]]
   [riichi-calc.ui.reagent.audio :as audio]
   [riichi-calc.ui.reagent.history :as history]
   [riichi-calc.ui.reagent.wizard :as wizard]
   [riichi-calc.ui.reagent.widget :as widget]
   [riichi-calc.ui.reagent.reagentState :as state]
   [riichi-calc.ui.reagent.svg :as svg]))

;(enable-console-print!)

(defonce *state (r/atom state/initial-state))

(defn settings-render []
  [:div#settings.field
   [:fieldset.field [:legend "Theme"]
    [widget/radio-group [:regular :black] (:theme @*state) #(swap! *state assoc :theme %)]]
   [:fieldset.field [:legend "Yaku Names Language"]
    [widget/radio-group [:ja :romaji :it :en] (:language @*state) #(swap! *state assoc :language %)]]])

(defn keyboard-mode-render []
   [:fieldset#keyboard-mode.field [:legend "Keyboard mode:"]
    (widget/radio-group [:an :chii :pon :kan :ankan :dorahyouji :agaripai]
                 (:keyboard-mode @*state)
                 #(swap! *state assoc :keyboard-mode %1))])

(defn keyboard-render []
  (let [{:keys [keyboard-mode hand theme]} @*state 
        enabled? (partial common-state/can-input? keyboard-mode hand)]
    [widget/keyboard *state theme tile/all-34-tiles-with-redfives enabled? state/keyboard-input!]))

(defn- advance-wind [wind]
  (swap! *state update-in [:hand wind] tile/wind-next))

(defn- wind-button [wind kind theme]
  [:div.tile-button [:div (s/capitalize (name kind))]
   (assoc-in (svg/tile theme wind) [1 :on-click] #(advance-wind kind))])

(defn new-hand! []
  (let [{:keys [hand]} @*state]
    (history/save-hand! hand)
    (reset! *state state/initial-state)))

(defn buttons []
  [:div.field.buttons
   [:button.button.is-info {:on-click wizard/open!} "Wizard"]
   [:button.button.is-primary {:on-click new-hand!} "New hand"]])

(defn hand-properties-render []
  (let [{:keys [hand theme language]} @*state]
    [:<>
     [buttons]
     [:div#hand-properties.field
      (wind-button (tile/wind (:bakaze hand)) :bakaze theme)
      (wind-button (tile/wind (:jikaze hand)) :jikaze theme)
      (widget/agaripai-view *state (:agaripai hand))
      (widget/dorahyouji *state hand)]
     [widget/agari *state (:agari hand)]
     [widget/extra-yaku *state (:extra-yaku hand) language]
     [widget/extra *state (get-in hand [:extra :dora]) (get-in hand [:extra :yaku])]]))

(defn speech-of-result [lang {:keys [yakus score]}]
  (str
   (s/join ". " (map (partial hand/string-of-yaku ({:romaji :ja} lang lang)) yakus))
   ". "
   (hand/speech-of-score score)))

(defn result-win [lang {:keys [yakus han fu score] :as result}]
  (let [actual-lang ({:romaji :ja} lang lang)]
    (audio/speak actual-lang (speech-of-result actual-lang result)))
  [:<>
   [:table.table.is-hoverable [:thead [:tr [:th "Yaku Name"] [:th "Han Value"]]]
    [:tbody
     (for [yaku yakus
           :let [wiki (get-in yakudb [(key yaku) :wiki])
                 name (get-in yakudb [(key yaku) :name lang] (s/capitalize (name (key yaku))))]]
       ^{:key (str (key yaku) (val yaku))}
       [:tr [:td (if (nil? wiki) name [:a {:href wiki :target "_blank"} name])] [:td (val yaku)]])
     [:tr.value [:td "Value"] [:td (hand/string-of-value han fu)]]
     [:tr.score [:td "Score"] [:td (hand/string-of-score score)]]]]
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

(defn from-notation-min [notation-min]
  (filter (some-fn group/tris? group/quad? group/straight?)
          (hand/grouped-tiles (hand/from-notation notation-min))))

(defn cljs-copy-to-clipboard
  "navigator.clipboard.writeText(text).then(function() {
    console.log('Async: Copying to clipboard was successful!');
  }, function(err) {
    console.error('Async: Could not copy text: ', err);
  });"
  [text]
  (. (js/navigator.clipboard.writeText text) then
         #(println "Copying to clipboard was successful!")
         #(println "Could not copy text: " %1)))

(defn notation-render []
  (let [notation (r/atom "")
        typing (r/atom false)]
    (fn []
      (let [{:keys [hand]} @*state
            [notation-an notation-min] (s/split @notation "|")
            hand-an (vec (hand/from-notation notation-an))
            hand-min (vec (from-notation-min notation-min))
            changed (or (not= (:an hand) hand-an) (not= (:min hand) hand-min))]
        (when changed
          (if @typing
            (do
              (swap! *state (fn [state]
                              (-> state
                                  (update :hand (fn [h] (assoc h :an hand-an :min hand-min)))
                                  (update :keyboard-mode (partial common-state/next-keyboard-mode state)))))
              (reset! typing false))
            (reset! notation (hand/to-notation hand)))))
      [:fieldset.field.has-addons
       [:legend "Notation"]
       [:div.control
        [:input.input {:type :text
                       :name "notation"
                       :value @notation
                       :onChange #(do (reset! notation (.. % -target -value)) (reset! typing true))}]]
       [:div.control
        [:input.button {:type :button
                        :name "notation-copy"
                        :value "Copy"
                        :onClick #(cljs-copy-to-clipboard @notation)
                        :disabled (empty? @notation)}]]])))

(defn restore-hand! [hand]
  (swap! *state assoc :hand hand))

(defn app-render []
  [:<>
   [hand-properties-render]
   [keyboard-render]
   [keyboard-mode-render]
   [widget/hand-render (:theme @*state) (:hand @*state) (partial state/remove-from-hand! *state)]
   [notation-render]
   [settings-render]
   [wizard/render *state]
   ])

(defonce root-interactive (createRoot (gdom/getElement "interactive")))
(defonce root-results (createRoot (gdom/getElement "results")))
(defonce root-history (createRoot (gdom/getElement "history")))

(defn init
  []
  (audio/play-tile-down-sfx)
  (history/init!)
  (.render root-interactive (r/as-element [app-render]))
  (.render root-results (r/as-element [results-render]))
  (.render root-history (r/as-element [(partial history/render (:theme @*state) widget/hand-render restore-hand!)])))

#_{:clojure-lsp/ignore [:clojure-lsp/unused-public-var]}
(defn ^:dev/after-load re-render
  []
  ;; The `:dev/after-load` metadata causes this function to be called
  ;; after shadow-cljs hot-reloads code.
  ;; This function is called implicitly by its annotation.
  (init))
