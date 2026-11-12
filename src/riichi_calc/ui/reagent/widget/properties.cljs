(ns riichi-calc.ui.reagent.widget.properties
  (:require
   [clojure.string :as s]
   [riichi-calc.hand :as hand]
   [riichi-calc.tile :as tile]
   [riichi-calc.ui.reagent.svg :as svg]
   [riichi-calc.ui.reagent.widget.base :as base]
   [riichi-calc.yakudb :refer [yakudb]]))

(defn- advance-wind [hand-cursor wind]
  (swap! hand-cursor update wind tile/wind-next))

(defn- wind-button [wind kind theme hand-cursor]
  [:div.tile-button [:div (s/capitalize (name kind))]
   (assoc-in (svg/tile theme wind) [1 :on-click] #(advance-wind hand-cursor kind))])

(defn agari [hand-cursor agari]
  [:fieldset.field [:legend "Agari:"]
   [base/radio-group "agari"
                [:tsumo :ron]
                agari
                #(swap! hand-cursor assoc :agari %1)]])

(defn agaripai-view [theme hand-cursor tile]
  [:div.tile-button [:div "Agaripai"]
   (assoc-in (svg/tile theme tile) [1 :on-click]
             #(swap! hand-cursor assoc :agaripai nil))])

(defn ^:private extra-yaku->checkboxes [extra-yaku lang]
  (let [yakus [:riichi :ippatsu :chankan :rinshan-kaihou :haitei-raoyue :houtei-raoyui]
        checked? #(contains? extra-yaku %)
        disabled? #(and (= % :ippatsu) (not (contains? extra-yaku :riichi)))]
    (zipmap yakus (map #(hash-map :name (get-in yakudb [% :name lang]) :checked (checked? %) :disabled (disabled? %)) yakus))))

(defn extra-yaku [lang hand-cursor extra-yaku]
  [:fieldset.field [:legend "Extra yakus:"]
   [base/checkboxes (extra-yaku->checkboxes extra-yaku lang)
    (fn [yaku checked]
      (swap! hand-cursor (if checked
                           #(hand/add-yaku % yaku)
                           #(hand/remove-yaku % yaku))))]])

(defn ^:private handle-extra-doras [hand-cursor event]
  (let [doras (int (.. event -nativeEvent -data))]
    (swap! hand-cursor assoc-in [:extra :dora] doras)))

(defn ^:private handle-extra-yakus [hand-cursor event]
  (let [yakus (int (.. event -nativeEvent -data))]
    (swap! hand-cursor assoc-in [:extra :yaku] yakus)))

(defn extra [hand-cursor doras yakus]
  [:fieldset.field.has-addons [:legend "Extras:"]
   [:div.control [:label {:for "extra-doras"} "Dora:"]
    [:input#extra-doras.input {:type :number :value doras :step 1 :min 0 :max 20
                               :on-change (partial handle-extra-doras hand-cursor)}]]
   [:div.control [:label {:for "extra-yakus"} "Yaku:"]
    [:input#extra-yakus.input {:type :number :value yakus :step 1 :min 0 :max 13
                               :on-change (partial handle-extra-yakus hand-cursor)}]]])

(defn dorahyouji-tile [theme hand-cursor tile index]
  (cond-> (svg/tile theme tile)
    tile (assoc-in [1 :on-click]
                   #(swap! hand-cursor hand/remove-from-hand :dorahyouji index))))

(defn dorahyouji [theme hand-cursor {:keys [extra-yaku dorahyouji]}]
  (let [doras (take 5 (lazy-cat dorahyouji (repeat nil)))
        uradoras (take 5 (lazy-cat (drop 5 dorahyouji) (repeat nil)))]
    [:div#dorahyouji [:div "Dorahyouji"]
     [:div.flex-column-2px
      [:div.tile-row (for [[index dora] (map-indexed vector doras)]
                       ^{:key (str index dora)} [dorahyouji-tile theme hand-cursor dora index])]
      (when (contains? extra-yaku :riichi)
        [:div.tile-row (for [[index dora] (map-indexed vector uradoras)]
                         ^{:key (str index dora)} [dorahyouji-tile theme hand-cursor dora index])])]]))

(defn hand-wind-agaripai-dorahyouji [theme hand hand-cursor]
  [:div#hand-properties.field
   (wind-button (tile/wind (:bakaze hand)) :bakaze theme hand-cursor)
   (wind-button (tile/wind (:jikaze hand)) :jikaze theme hand-cursor)
   [agaripai-view theme hand-cursor (:agaripai hand)]
   [dorahyouji theme hand-cursor hand]])

(defn hand-properties [theme language hand-cursor]
  (let [hand @hand-cursor]
    [:<>
     [hand-wind-agaripai-dorahyouji theme hand hand-cursor]
     [agari hand-cursor (:agari hand)]
     [extra-yaku language hand-cursor (:extra-yaku hand)]
     [extra hand-cursor (get-in hand [:extra :dora]) (get-in hand [:extra :yaku])]]))
