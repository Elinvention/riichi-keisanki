(ns riichi-calc.ui.reagent.widget.results
  (:require
   [clojure.string :as s]
   [reagent.core :as r]
   [riichi-calc.hand :as hand]
   [riichi-calc.state :as common-state]
   [riichi-calc.tile :as tile]
   [riichi-calc.ui.reagent.audio :as audio]
   [riichi-calc.ui.reagent.svg :as svg]
   [riichi-calc.yakudb :refer [yakudb]]))

(def ^:private table-fu [20 25 30 40 50 60 70 80 90 100 110])
(def ^:private table-han [1 2 3 4 5 6 8 11 13 "+"])
(def ^:private cap-to-bulma-class {:mangan 30 :haneman 25 :baiman 20 :sanbaiman 15 :yakuman 10})

(defn- score-cap-to-cell-class [score-cap]
  (let [c (cap-to-bulma-class score-cap)]
    (str "has-text-info-" c " has-background-info-" c "-invert")))

(defn score-table-render
  "Renders the table of scores for reference and highlight current score"
  [{:keys [jikaze agari han fu] :as score}]
  (let [cols (count table-fu)
        dealer? (= jikaze :east)
        current-han (:regular han)
        current-fu (hand/final-minipoints fu)
        highlight? (fn [h f cap]
                     (if (some? cap)
                       (or (and (= h current-han) (>= current-fu f))
                           (and (>= h 13) (some? (:yakuman han))))
                       (and (= h current-han) (= current-fu f))))]
    [:div.table-container
     [:table.table.is-hoverable
      [:thead
       [:tr
        [:th {:colSpan (inc cols)}
         (str (if dealer? "Dealer " "Non-dealer ") (name agari) " score")]]
       [:tr [:th "Han"] (for [f table-fu] [:th {:key (str f)} f])]]
      [:tbody
       (for [[h next] (partition 2 1 table-han)]
         [:tr {:key (str h agari jikaze)}
          [:td (if (number? next) (s/join ", " (range h next)) "13+")]
          (let [*capped-count (atom 0)]
            (for [[i f] (map-indexed vector table-fu)
                  :while (= 0 @*capped-count)
                  :let [current-split-pay (hand/split-pay jikaze agari {:regular h} f)
                        current-total-pay (hand/total-pay current-split-pay)
                        score-cap (hand/score-cap jikaze current-total-pay)
                        test-score (assoc score :split current-split-pay
                                          :total current-total-pay
                                          :cap score-cap)
                        cell-content (hand/string-of-score-compact test-score)
                        cell-class (if (some #(highlight? % f score-cap) (range h next))
                                     "is-selected"
                                     ((fnil score-cap-to-cell-class "") score-cap))
                        colspan (when (some? score-cap)
                                  (swap! *capped-count inc)
                                  (- cols i))]]
              [:td {:key (str agari jikaze h f)
                    :class (str cell-class " has-text-centered")
                    :colSpan colspan}
               cell-content]))])]]]))

(defn results-table-render [lang {:keys [yakus han fu score]}]
  [:div.box
   [:nav.level
    (for [yaku yakus
          :let [wiki (get-in yakudb [(key yaku) :wiki])
                yaku-name (get-in yakudb [(key yaku) :name lang] (s/capitalize (name (key yaku))))
                value (if (integer? (val yaku))
                        (val yaku)
                        (s/capitalize (name (val yaku))))]]
      ^{:key (str (key yaku) (val yaku))}
      [:div.level-item.has-text-centered
       [:div
        [:p.heading (if (nil? wiki) yaku-name [:a {:href wiki :target "_blank"} yaku-name])]
        [:p.title value]]])]
   [:hr]
   [:nav.level
    (if (:yakuman han)
      [:div.level-item.has-text-centered
       [:div
        [:p.heading "Han"]
        [:p.title (hand/string-of-value han fu)]]]
      [:<>
       [:div.level-item.has-text-centered
        [:div
         [:p.heading "Han"]
         [:p.title (:regular han)]]]
       [:div.level-item.has-text-centered {:title (hand/explain-minipoints fu)}
        [:div
         [:p.heading "Fu"]
         [:p.title (hand/final-minipoints fu)]]]])
    [:div.level-item.has-text-centered
     [:div
      [:p.heading "Score"]
      [:p.title (hand/string-of-score score)]]]]])

(defn speech-of-result [lang {:keys [yakus score]}]
  (str
   (s/join ". " (map (partial hand/string-of-yaku (get {:romaji :ja} lang lang)) yakus))
   ". "
   (hand/speech-of-score score)))

(defn button-play-results-speech [lang result]
  [:input.button.block
   {:type :button
    :on-click (fn []
                (let [actual-lang (get {:romaji :ja} lang lang)
                      speech (speech-of-result lang result)]
                  (audio/speak actual-lang speech)))
    :value "Play results speech ▶️"}])

(defn- result-win [lang {:keys [score] :as result}]
  [:<>
   [button-play-results-speech lang result]
   [results-table-render lang result]
   [score-table-render score]])

(defn- ukeire-tile [theme tile *state]
  (assoc-in (svg/tile theme tile) [1 :on-click]
            #(swap! *state common-state/add-agaripai tile)))

(defn- result-tenpai [theme *state {:keys [ukeire summary]}]
  [:<>
   [:p summary]
   [:div.tile-row
    (for [tile (tile/sort-tiles ukeire)]
      ^{:key (str "tenpai" theme (tile/tile-name tile))}
      [ukeire-tile theme tile *state])]])

(defn- agaripai-tile [theme tile *state]
  (assoc-in (svg/tile theme tile) [1 :on-click]
            #(swap! *state common-state/set-agaripai tile)))

(defn- result-agaripai [theme *state {:keys [an] :as res}]
  [:<>
   [:p (:summary res)]
   [:div.tile-row
    (for [tile (dedupe (tile/sort-tiles an))]
      ^{:key (str "agaripai" theme (tile/tile-name tile))}
      [agaripai-tile theme tile *state])]])

(defn render [*state]
  (let [{:keys [hand theme language]} @*state
        {:keys [summary] :as res} (hand/results hand language)]
    (case (:type res)
      (:incomplete :invalid :no-yaku) [:p summary]
      :agaripai [result-agaripai theme *state res]
      :tenpai [result-tenpai theme *state res]
      :winning [result-win language res])))
