(ns riichi-calc.reagent
  (:require [clojure.string :refer [capitalize]]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [riichi-calc.group :as group]
            [riichi-calc.hand :as hand]
            [riichi-calc.state :as state]
            [riichi-calc.tile :as tile]
            [riichi-calc.yakudb :refer [yakudb]]))


(def *state (r/atom state/initial-state))

(defn url-from-name [theme tname]
  (str "resources/tiles/" (capitalize (name theme)) "/" tname ".svg"))

(defn url [theme tile]
  (url-from-name theme (tile/tile-name tile)))

(defn front-tile [theme]
  [:image {:width 54 :xlinkHref (url-from-name theme "Front")
           :on-drag-start #(.preventDefault %)}])

(defn back-tile [theme]
  [:image {:width 54 :xlinkHref (url-from-name theme "Back")
           :on-drag-start #(.preventDefault %)}])

(defn svg-tile [theme tile rotated]
  [:svg.tile {:xmlns "http://www.w3.org/2000/svg"
              :xmlnsXlink "http://www.w3.org/1999/xlink"
              :view-box "0 0 54 72"
              :transform (if rotated "rotate(90)" "")
              :style (if rotated {:margin "0 10px 0 10px"} {})}
   (if (some? tile)
     [:g
      [front-tile theme]
      [:image {:xlinkHref (url theme tile)
               :width 45
               :transform "translate(4, 5)"
               :on-drag-start #(.preventDefault %)}]]
     [back-tile theme])])

(defn remove-from-hand [path index]
  (swap! *state update :hand hand/remove-from-hand path index))

(defn play-tile-down-sfx []
  (let [domaudio (js/document.getElementById "klick4")]
    (set! (.-currentTime domaudio) 0)
    (-> (.play domaudio)
        (.catch #(println "Can't play sound:" (. % -message))))))

(def update-hand-with-sfx (partial state/update-hand-with-sfx play-tile-down-sfx))

(defn keyboard-key [layout tile enabled]
  (let [plain-tile (svg-tile layout tile false)]
    (if enabled
      (assoc-in plain-tile [1 :on-click] #(swap! *state state/keyboard-input tile update-hand-with-sfx))
      (update-in plain-tile [1 :style] assoc :opacity "50%"))))

(defn radio-group [options value on-change]
  [:<>
   (for [option options
         :let [radio-name (name option)
               id (str radio-name "-radiobutton")]]
     ^{:key option} [:span
                     [:input {:type :radio
                              :id id
                              :name radio-name
                              :checked (= option value)
                              :on-change #(on-change option)}]
                     [:label {:for id} (capitalize radio-name)]])])

(defn checkboxes [boxes on-change]
  [:<>
   (for [box boxes
         :let [bname (get (val box) :name)
               id (str bname "-checkbox")
               checked (get (val box) :checked)
               disabled (get (val box) :disabled)
               closure #(on-change (key box) (not checked))]]
     ^{:key (str bname (val box))}
     [:span
      [:input {:type :checkbox
               :name bname
               :value bname
               :id id
               :checked checked
               :disabled disabled
               :on-change closure}]
      [:label {:for id} (capitalize bname)]])])

(defn settings-render []
  [:<>
   [:fieldset [:legend "Theme"]
    [radio-group [:regular :black] (:theme @*state) #(swap! *state assoc :theme %)]]
   [:fieldset [:legend "Yaku Names Language"]
    [radio-group [:ja :romaji :it :en] (:language @*state) #(swap! *state assoc :language %)]]])

(defn keyboard-mode-render []
  [:fieldset [:legend "Keyboard mode:"]
   (radio-group [:an :chii :pon :kan :ankan :dorahyouji :agaripai]
                (:keyboard-mode @*state)
                #(swap! *state assoc :keyboard-mode %1))
   (checkboxes {:akadora {:name "Akadora" :checked (:akadora @*state)}} #(swap! *state update %1 not))])

(defn agari-widget [agari]
  [:fieldset [:legend "Agari:"]
   (radio-group [:tsumo :ron]
                agari
                #(swap! *state assoc-in [:hand :agari] %1))])

(defn extra-yaku->checkboxes [extra-yaku lang]
  (let [yakus [:riichi :ippatsu :chankan :rinshan-kaihou :haitei-raoyue :houtei-raoyui]
        checked? #(contains? extra-yaku %)
        disabled? #(and (= % :ippatsu) (not (contains? extra-yaku :riichi)))]
    (zipmap yakus (map #(hash-map :name (get-in yakudb [% :name lang]) :checked (checked? %) :disabled (disabled? %)) yakus))))

(defn extra-yaku-widget [extra-yaku lang]
  [:fieldset [:legend "Extra yakus:"]
   [checkboxes (extra-yaku->checkboxes extra-yaku lang)
    (fn [yaku checked]
      (swap! *state update :hand #(if checked
                                    (hand/add-yaku %1 yaku)
                                    (hand/remove-yaku %1 yaku))))]])

(defn keyboard-render []
  (let [{:keys [keyboard-mode akadora hand theme]} @*state
        ukeire (hand/ukeire hand)
        key-tiles (for [t tile/all-34-tiles
                        :let [akat (if (and akadora (= 5 (:value t)))
                                     (assoc t :red true) t)
                              enabled (state/can-input? keyboard-mode akadora hand akat ukeire)]]
                    ^{:key (str (url theme akat) enabled)}
                    [keyboard-key theme akat enabled])]
      [:<>
       (for [nine-tile (partition 9 9 nil key-tiles)]
         ^{:key nine-tile} [:span.tile-row nine-tile])]))

(defn hand-tile [tile path pos rotated dora]
  (cond-> (svg-tile (:theme @*state) tile rotated)
    true (assoc-in [1 :on-click] #(remove-from-hand path pos))
    dora (assoc-in [1 :class] "dora")))

(defn agaripai-view [tile]
  [:div.tile-button [:div "Agaripai"]
   (assoc-in (svg-tile (:theme @*state) tile false) [1 :on-click]
             #(swap! *state assoc-in [:hand :agaripai] nil))])

(defn dorahyouji-tile [tile index]
  (cond-> (svg-tile (:theme @*state) tile false)
    tile (assoc-in [1 :on-click] #(remove-from-hand :dorahyouji index))))

(defn dorahyouji-widget [{:keys [extra-yaku dorahyouji]}]
  (let [doras (take 5 (lazy-cat dorahyouji (repeat nil)))
        uradoras (take 5 (lazy-cat (drop 5 dorahyouji) (repeat nil)))]
    [:div#dorahyouji [:div "Dorahyouji"]
     [:div.tile-row (for [[index dora] (map-indexed vector doras)]
                      ^{:key (str index dora)} [dorahyouji-tile dora index])]
     (when (contains? extra-yaku :riichi)
       [:div.tile-row (for [[index dora] (map-indexed vector uradoras)]
                        ^{:key (str index dora)} [dorahyouji-tile dora index])])]))

(defn- advance-wind [wind]
  (swap! *state update-in [:hand wind] tile/wind-next))

(defn- wind-button [wind kind theme]
  [:div.tile-button [:div (capitalize (name kind))]
   (assoc-in (svg-tile theme wind false) [1 :on-click] #(advance-wind kind))])

(defn- hand-an-render [{:keys [an] :as hand}]
  (reduce
   (fn [val [i group-or-tile]]
     (if (group/group? group-or-tile)
       (concat val
               (for [[j tile] (map-indexed vector (group/expand group-or-tile))]
                 ^{:key (str "an" tile i j)}
                 [hand-tile (when (< 0 j 3) tile) :an i false (hand/dora? hand tile)]))
       (conj val
             ^{:key (str "an" group-or-tile i)}
             [hand-tile group-or-tile :an i false (hand/dora? hand group-or-tile)])))
   []
   (map-indexed vector an)))

(defn- hand-min-render [{:keys [min] :as hand}]
  (for [[index group] (map-indexed vector min)
        [i tile] (map-indexed vector (group/expand group))]
        ;;{:fx/type min-view :tile tile :index index :rotate (if (= i 0) 90 0) :theme theme}
    ^{:key (str "min" tile index i)}
    [hand-tile tile :min index (= i 0) (hand/dora? hand tile)]))

(defn hand-render []
  (let [{:keys [hand]} @*state]
    [:<> (concat (hand-an-render hand) (hand-min-render hand))]))

(defn hand-properties-render []
  (let [{:keys [hand theme language]} @*state]
    [:<>
     (wind-button (tile/wind (:bakaze hand)) :bakaze theme)
     (wind-button (tile/wind (:jikaze hand)) :jikaze theme)
     (agaripai-view (:agaripai hand))
     (dorahyouji-widget hand)
     [agari-widget (:agari hand)]
     [extra-yaku-widget (:extra-yaku hand) language]
     [:button {:on-click #(reset! *state state/initial-state)} "Reset"]]))

(defn result-win [lang {:keys [yakus han fu score]}]
  [:table [:thead [:tr [:th "Yaku Name"] [:th "Han Value"]]]
   [:tbody
    (for [yaku yakus
          :let [wiki (get-in yakudb [(key yaku) :wiki])
                name (get-in yakudb [(key yaku) :name lang] (capitalize (name (key yaku))))]]
      ^{:key (str (key yaku) (val yaku))}
      [:tr [:td (if (nil? wiki) name [:a {:href wiki :target "_blank"} name])] [:td (val yaku)]])
    [:tr.total [:td "Total"] [:td (hand/string-of-han han fu)]]
    [:tr.score [:td "Score"] [:td (hand/string-of-score score)]]]])

(defn ukeire-tile [theme tile]
  [assoc-in (svg-tile theme tile false) [1 :on-click]
   #(swap! *state state/add-agaripai tile)])

(defn result-tenpai [theme {:keys [ukeire summary]}]
  [:<>
   [:p summary]
   [:div.tile-row
    (for [tile (tile/sort-tiles ukeire)]
      ^{:key (str "tenpai" theme (tile/tile-name tile))}
      [ukeire-tile theme tile])]])

(defn results-render []
  (let [{:keys [hand theme language]} @*state
        {:keys [summary] :as res} (hand/results hand language)]
    (case (:type res)
      (:incomplete :invalid :agaripai :no-yaku) [:p summary]
      :tenpai (result-tenpai theme res)
      :winning (result-win language res))))

(defn ^:export run []
  (play-tile-down-sfx)
  (rdom/render [hand-properties-render] (js/document.getElementById "hand-properties"))
  (rdom/render [keyboard-render] (js/document.getElementById "keyboard"))
  (rdom/render [keyboard-mode-render] (js/document.getElementById "keyboard-mode"))
  (rdom/render [hand-render] (js/document.getElementById "hand"))
  (rdom/render [settings-render] (js/document.getElementById "settings"))
  (rdom/render [results-render] (js/document.getElementById "results")))

(run)
