(ns riichi-calc.ui.reagent.widget
  (:require
   [clojure.string :as s]
   [riichi-calc.hand :as hand]
   [riichi-calc.group :as group]
   [riichi-calc.yakudb :refer [yakudb]]
   [riichi-calc.ui.reagent.svg :as svg]
   [riichi-calc.state :as state]
   [riichi-calc.ui.reagent.audio :as audio]))

(defn radio-group [options value on-change]
  [:div.radios
   (for [option options]
     ^{:key option} [:label.radio
                     [:input {:type :radio
                              :name (name option)
                              :checked (= option value)
                              :on-change #(on-change option)}]
                     (s/capitalize (name option))])])

(defn checkboxes [boxes on-change]
  [:div.checkboxes
   (for [box boxes
         :let [bname (get (val box) :name)
               checked (get (val box) :checked)
               disabled (get (val box) :disabled)
               closure #(on-change (key box) (not checked))]]
     ^{:key (str bname (val box))}
     [:label.checkbox.mr-2
      [:input {:type :checkbox
               :name bname
               :value bname
               :checked checked
               :disabled disabled
               :on-change closure}]
      " " (s/capitalize bname)])])

(defn keyboard-key [*state update-state-fn! theme tile enabled]
  (let [plain-tile (svg/tile theme tile)]
    (if enabled
      (assoc-in plain-tile [1 :on-click] #(update-state-fn! *state tile))
      (update-in plain-tile [1 :style] assoc :opacity "50%"))))

(defn keyboard [*state theme tiles enabled? update-state-fn!]
  (let [key-tiles (for [tile tiles
                        :let [enabled (enabled? tile)]]
                    ^{:key (str (svg/url theme tile) enabled)}
                    [(partial keyboard-key *state update-state-fn!) theme tile enabled])]
    [:div.keyboard
     (for [tile-row (partition 10 10 nil key-tiles)]
       ^{:key tile-row} [:span.tile-row tile-row])]))

(defn agari [*state agari]
  [:fieldset.field [:legend "Agari:"]
   (radio-group [:tsumo :ron]
                agari
                #(swap! *state assoc-in [:hand :agari] %1))])

(defn agaripai-view [*state tile]
  [:div.tile-button [:div "Agaripai"]
   (assoc-in (svg/tile (:theme @*state) tile) [1 :on-click]
             #(swap! *state assoc-in [:hand :agaripai] nil))])

(defn ^:private extra-yaku->checkboxes [extra-yaku lang]
  (let [yakus [:riichi :ippatsu :chankan :rinshan-kaihou :haitei-raoyue :houtei-raoyui]
        checked? #(contains? extra-yaku %)
        disabled? #(and (= % :ippatsu) (not (contains? extra-yaku :riichi)))]
    (zipmap yakus (map #(hash-map :name (get-in yakudb [% :name lang]) :checked (checked? %) :disabled (disabled? %)) yakus))))

(defn extra-yaku [*state extra-yaku lang]
  [:fieldset.field [:legend "Extra yakus:"]
   [checkboxes (extra-yaku->checkboxes extra-yaku lang)
    (fn [yaku checked]
      (swap! *state update :hand #(if checked
                                    (hand/add-yaku %1 yaku)
                                    (hand/remove-yaku %1 yaku))))]])

(defn ^:private handle-extra-doras [*state event]
  (let [doras (int (.. event -nativeEvent -data))]
    (state/set-extra-dora! *state doras)))

(defn ^:private handle-extra-yakus [*state event]
  (let [yakus (int (.. event -nativeEvent -data))]
    (state/set-extra-yaku! *state yakus)))

(defn extra [*state doras yakus]
  (println "extra-widget" doras yakus)
  [:fieldset.field.has-addons [:legend "Extras:"]
   [:div.control [:label {:for "extra-doras"} "Dora:"]
    [:input#extra-doras.input {:type :number :value doras :step 1 :min 0 :max 20
                               :on-change (partial handle-extra-doras *state)}]]
   [:div.control [:label {:for "extra-yakus"} "Yaku:"]
    [:input#extra-yakus.input {:type :number :value yakus :step 1 :min 0 :max 13
                               :on-change (partial handle-extra-yakus *state)}]]])

(defn dorahyouji-tile [*state tile index]
  (cond-> (svg/tile (:theme @*state) tile)
    tile (assoc-in [1 :on-click]
                   #(swap! *state update :hand hand/remove-from-hand :dorahyouji index))))

(defn dorahyouji [*state {:keys [extra-yaku dorahyouji]}]
  (let [doras (take 5 (lazy-cat dorahyouji (repeat nil)))
        uradoras (take 5 (lazy-cat (drop 5 dorahyouji) (repeat nil)))]
    [:div#dorahyouji [:div "Dorahyouji"]
     [:div.flex-column-2px
      [:div.tile-row (for [[index dora] (map-indexed vector doras)]
                       ^{:key (str index dora)} [dorahyouji-tile *state dora index])]
      (when (contains? extra-yaku :riichi)
        [:div.tile-row (for [[index dora] (map-indexed vector uradoras)]
                         ^{:key (str index dora)} [dorahyouji-tile *state dora index])])]]))

(defn hand-tile [svg-tile-fn theme tile on-click path pos dora]
  (cond-> (svg-tile-fn theme tile)
    (fn? on-click) (assoc-in [1 :on-click] #(on-click path pos))
    dora (assoc-in [1 :class] "dora")))

(defn- hand-an-render [theme {:keys [an] :as hand} on-click]
  (reduce
   (fn [val [i group-or-tile]]
     (if (group/group? group-or-tile)
       (concat val
               (for [[j tile] (map-indexed vector (group/expand group-or-tile))]
                 ^{:key (str "an" tile i j)}
                 [hand-tile svg/tile theme (when (< 0 j 3) tile) on-click :an i (hand/dora? hand tile)]))
       (conj val
             ^{:key (str "an" group-or-tile i)}
             [hand-tile svg/tile theme group-or-tile on-click :an i (hand/dora? hand group-or-tile)])))
   []
   (map-indexed vector an)))

(defn- hand-min-render [theme {:keys [min] :as hand} on-click]
  (for [[index group] (map-indexed vector min)
        [i tile] (map-indexed vector (group/expand group))]
    ;;{:fx/type min-view :tile tile :index index :rotate (if (= i 0) 90 0) :theme theme}
    ^{:key (str "min" tile index i)}
    [hand-tile (if (= i 0) svg/tile-rotated svg/tile) theme tile on-click :min index (hand/dora? hand tile)]))

(defn hand-render [theme hand on-click]
  [:div.hand (concat (hand-an-render theme hand on-click) (hand-min-render theme hand on-click))])

(def table-fu [20 25 30 40 50 60 70 80 90 100 110])
(def table-han [1 2 3 4 5 6 8 11 13 "+"])
(def cap-to-bulma-class {:mangan 30 :haneman 25 :baiman 20 :sanbaiman 15 :yakuman 10})
(defn score-cap-to-cell-class [score-cap]
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
  [:table.table.is-hoverable [:thead [:tr [:th "Yaku Name"] [:th "Han Value"]]]
   [:tbody
    (for [yaku yakus
          :let [wiki (get-in yakudb [(key yaku) :wiki])
                name (get-in yakudb [(key yaku) :name lang] (s/capitalize (name (key yaku))))]]
      ^{:key (str (key yaku) (val yaku))}
      [:tr [:td (if (nil? wiki) name [:a {:href wiki :target "_blank"} name])] [:td (val yaku)]])
    [:tr.value [:td "Value"] [:td (hand/string-of-value han fu)]]
    [:tr.score [:td "Score"] [:td (hand/string-of-score score)]]]])

(defn speech-of-result [lang {:keys [yakus score]}]
  (str
   (s/join ". " (map (partial hand/string-of-yaku ({:romaji :ja} lang lang)) yakus))
   ". "
   (hand/speech-of-score score)))

(defn button-play-results-speech [lang result]
  (println lang result)
  [:input.button.block
   {:type :button
    :on-click (fn []
                (let [actual-lang ({:romaji :ja} lang lang)
                      speech (speech-of-result lang result)]
                  (audio/speak actual-lang speech)
                  (println "playing speech" speech)))
    :value "Play results speech ▶️"}])
