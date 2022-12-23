(ns riichi-calc.reagent
  (:require [clojure.string :refer [capitalize]]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [riichi-calc.group :as group]
            [riichi-calc.hand :as hand]
            [riichi-calc.state :as state]
            [riichi-calc.tile :as tile]
            [riichi-calc.yakudb :refer [yakudb]]
            [goog.string :as gstring]))

;(enable-console-print!)

(defonce *state (r/atom state/initial-state))

(def tile-width 48)
(def tile-height 64)

(defn url-from-name [theme tname]
  (str "assets/tiles/" (capitalize (name theme)) "/" tname ".svg"))

(defn url [theme tile]
  (url-from-name theme (tile/tile-name tile)))

(defn front-tile-bg [theme]
  [:image {:width tile-width :xlinkHref (url-from-name theme "Front")
           :on-drag-start #(.preventDefault %)}])

(defn front-tile-fg [theme tile]
  [:image {:xlinkHref (url theme tile)
           :width 40
           :transform "translate(4, 5)"
           :on-drag-start #(.preventDefault %)}])

(defn back-tile-bg [theme]
  [:image {:width tile-width :xlinkHref (url-from-name theme "Back")
           :on-drag-start #(.preventDefault %)}])

(defn front-tile [theme tile]
  [:g [front-tile-bg theme] [front-tile-fg theme tile]])

(def svg-tile-container
  [:svg.mahjong-tile {:xmlns "http://www.w3.org/2000/svg"
              :xmlnsXlink "http://www.w3.org/1999/xlink"
              :view-box (str "0 0 " tile-width " " tile-height)}])

(defn svg-tile [theme tile]
  (conj svg-tile-container (if (some? tile) (front-tile theme tile) (back-tile-bg theme))))

(defn svg-tile-rotated [theme tile]
  [:span.rotated {} (svg-tile theme tile)])

(defn svg-tile-fg [theme tile]
  (conj svg-tile-container (front-tile-fg theme tile)))

(defn remove-from-hand [path index]
  (swap! *state update :hand hand/remove-from-hand path index))

(defn play-tile-down-sfx []
  (let [domaudio (js/document.getElementById "klick4")]
    (set! (.-currentTime domaudio) 0)
    (-> (.play domaudio)
        (.catch #(println "Can't play sound:" (. % -message))))))

(def update-hand-with-sfx (partial state/update-hand-with-sfx play-tile-down-sfx))

(def keyboard-input (partial state/keyboard-input update-hand-with-sfx))

(defn keyboard-key [update-state-fn theme tile enabled]
  (let [plain-tile (svg-tile theme tile)]
    (if enabled
      (assoc-in plain-tile [1 :on-click] #(swap! *state update-state-fn tile))
      (update-in plain-tile [1 :style] assoc :opacity "50%"))))

(defn radio-group [options value on-change]
  [:<>
    (for [option options]
     ^{:key option} [:label.radio
                     [:input {:type :radio
                              :name (name option)
                              :checked (= option value)
                              :on-change #(on-change option)}]
                     (capitalize (name option))])])

(defn checkboxes [boxes on-change]
  [:div.control
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
      " " (capitalize bname)])])

(defn settings-render []
  [:div#settings
   [:fieldset.field [:legend "Theme"]
    [radio-group [:regular :black] (:theme @*state) #(swap! *state assoc :theme %)]]
   [:fieldset.field [:legend "Yaku Names Language"]
    [radio-group [:ja :romaji :it :en] (:language @*state) #(swap! *state assoc :language %)]]])

(defn keyboard-mode-render []
   [:fieldset#keyboard-mode.field [:legend "Keyboard mode:"]
    (radio-group [:an :chii :pon :kan :ankan :dorahyouji :agaripai]
                 (:keyboard-mode @*state)
                 #(swap! *state assoc :keyboard-mode %1))])

(defn agari-widget [agari]
  [:fieldset.field [:legend "Agari:"]
   (radio-group [:tsumo :ron]
                agari
                #(swap! *state assoc-in [:hand :agari] %1))])

(defn extra-yaku->checkboxes [extra-yaku lang]
  (let [yakus [:riichi :ippatsu :chankan :rinshan-kaihou :haitei-raoyue :houtei-raoyui]
        checked? #(contains? extra-yaku %)
        disabled? #(and (= % :ippatsu) (not (contains? extra-yaku :riichi)))]
    (zipmap yakus (map #(hash-map :name (get-in yakudb [% :name lang]) :checked (checked? %) :disabled (disabled? %)) yakus))))

(defn extra-yaku-widget [extra-yaku lang]
  [:fieldset.field [:legend "Extra yakus:"]
   [checkboxes (extra-yaku->checkboxes extra-yaku lang)
    (fn [yaku checked]
      (swap! *state update :hand #(if checked
                                    (hand/add-yaku %1 yaku)
                                    (hand/remove-yaku %1 yaku))))]])

(defn keyboard-widget [theme tiles enabled? update-state-fn]
  (let [key-tiles (for [tile tiles
                        :let [enabled (enabled? tile)]]
                    ^{:key (str (url theme tile) enabled)}
                    [(partial keyboard-key update-state-fn) theme tile enabled])]
    [:div.keyboard
     (for [tile-row (partition 10 10 nil key-tiles)]
       ^{:key tile-row} [:span.tile-row tile-row])]))

(defn keyboard-render []
  (let [{:keys [keyboard-mode hand theme]} @*state 
        enabled? (partial state/can-input? keyboard-mode hand)]
    [keyboard-widget theme tile/all-34-tiles-with-redfives enabled? keyboard-input]))

(defn hand-tile [tile path pos svg-tile-fn dora]
  (cond-> (svg-tile-fn (:theme @*state) tile)
    true (assoc-in [1 :on-click] #(remove-from-hand path pos))
    dora (assoc-in [1 :class] "dora")))

(defn agaripai-view [tile]
  [:div.tile-button [:div "Agaripai"]
   (assoc-in (svg-tile (:theme @*state) tile) [1 :on-click]
             #(swap! *state assoc-in [:hand :agaripai] nil))])

(defn dorahyouji-tile [tile index]
  (cond-> (svg-tile (:theme @*state) tile)
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
   (assoc-in (svg-tile theme wind) [1 :on-click] #(advance-wind kind))])

(defn- hand-an-render [{:keys [an] :as hand}]
  (reduce
   (fn [val [i group-or-tile]]
     (if (group/group? group-or-tile)
       (concat val
               (for [[j tile] (map-indexed vector (group/expand group-or-tile))]
                 ^{:key (str "an" tile i j)}
                 [hand-tile (when (< 0 j 3) tile) :an i svg-tile (hand/dora? hand tile)]))
       (conj val
             ^{:key (str "an" group-or-tile i)}
             [hand-tile group-or-tile :an i svg-tile (hand/dora? hand group-or-tile)])))
   []
   (map-indexed vector an)))

(defn- hand-min-render [{:keys [min] :as hand}]
  (for [[index group] (map-indexed vector min)
        [i tile] (map-indexed vector (group/expand group))]
        ;;{:fx/type min-view :tile tile :index index :rotate (if (= i 0) 90 0) :theme theme}
    ^{:key (str "min" tile index i)}
    [hand-tile tile :min index (if (= i 0) svg-tile-rotated svg-tile) (hand/dora? hand tile)]))

(defn hand-render []
  (let [{:keys [hand]} @*state]
    [:div.hand (concat (hand-an-render hand) (hand-min-render hand))]))

(defn wizard-close! []
  (swap! *state assoc-in [:wizard :open] false))

(defn wizard-open! []
  (swap! *state assoc-in [:wizard :open] true))

(defn wizard-toggle! []
  (swap! *state update-in [:wizard :open] not))

(defn buttons []
  [:div.field
   [:button.button.is-primary.mr-1 {:on-click wizard-open!} "Wizard"]
   [:button.button.is-danger {:on-click #(reset! *state state/initial-state)} "Reset"]])

(defn hand-properties-render []
  (let [{:keys [hand theme language]} @*state]
    [:<>
     [buttons]
     [:div#hand-properties.field
      (wind-button (tile/wind (:bakaze hand)) :bakaze theme)
      (wind-button (tile/wind (:jikaze hand)) :jikaze theme)
      (agaripai-view (:agaripai hand))
      (dorahyouji-widget hand)]
     [agari-widget (:agari hand)]
     [extra-yaku-widget (:extra-yaku hand) language]]))



(defn wizard-steps [current-step {:keys [theme hand]}]
  [:ul.steps.has-content-centered.is-horizontal
   (let [steps [{:icon (svg-tile-fg theme (tile/wind (:jikaze hand))) :title "Jikaze"}
                {:icon (svg-tile-fg theme (tile/wind (:bakaze hand))) :title "Bakaze"}
                {:icon nil :title (gstring/unescapeEntities "Dora&shy;hyouji")}
                {:icon nil :title "Closed hand"}
                {:icon nil :title "Open hand"}
                {:icon nil :title "Agari"}]
         steps-indexed (map-indexed vector steps)]
     (for [[step {:keys [icon title]}] steps-indexed]
       ^{:key step}
       [:li.steps-segment {:class (when (= (inc step) current-step) "is-active")}
        [:span.steps-marker (when (some? icon) [:span.icon icon])]
        [:span.steps-content [:p {:style {:margin-top "8px"}} title]]]))])

(defn wizard-step-prev! []
  (swap! *state update-in [:wizard :step] (comp (partial max 1) dec)))

(defn wizard-step-next! []
  (swap! *state update-in [:wizard :step] (comp (partial min 6) inc)))

(defn agari! [agari]
  (swap! *state assoc-in [:hand :agari] agari))

(def ron! (partial agari! :ron))

(def tsumo! (partial agari! :tsumo))

(defn wizard-nav [step]
  [:div.card-footer
   (when (> step 1) [:button.card-footer-item.button {:on-click wizard-step-prev!} "Previous step"])
   (if (< step 6)
     [:button.card-footer-item.button.is-primary {:on-click wizard-step-next!} "Next step"]
     [:<>
      [:button.card-footer-item.button.is-danger {:on-click (comp wizard-close! ron!)} "Ron"]
      [:button.card-footer-item.button.is-success {:on-click (comp wizard-close! tsumo!)} "Tsumo"]])])

(defn wizard-wind-keyboard [theme kaze]
  [keyboard-widget theme tile/wind-tiles (constantly true) #(assoc-in %1 [:hand kaze] (:value %2))])

(def pon-conj! (partial state/pon-conj update-hand-with-sfx))
(def an-conj! (partial state/an-conj update-hand-with-sfx))
(def dorahyouji-conj! (partial state/dorahyouji-conj update-hand-with-sfx))

(defn wizard-agaripai-keyboard [theme hand]
  [keyboard-widget theme tile/all-34-tiles-with-redfives (partial hand/can-agaripai? hand) state/set-agaripai])

(defn wizard-open-hand-keyboard [theme hand]
  [keyboard-widget theme tile/all-34-tiles-with-redfives (partial hand/can-add-pon? hand) pon-conj!])

(defn wizard-closed-hand-keyboard [theme hand]
  [keyboard-widget theme tile/all-34-tiles-with-redfives (partial hand/can-add-tile? hand) an-conj!])

(defn dorahyouji-keyboard [theme hand]
  [keyboard-widget theme tile/all-34-tiles-with-redfives (partial hand/can-add-dorahyouji? hand) dorahyouji-conj!])

(defn wizard-render []
  (let [{:keys [hand wizard theme]} @*state]
    [(if (:open wizard) :div#wizard.modal.is-active :div#wizard.modal)
     [:div.modal-background {:on-click #(swap! *state assoc-in [:wizard :open] false)}]
     [:div.modal-content
      [:div.card
       [:div.card-header [:p.card-header-title "Wizard"]]
       [:div.card-content
        [wizard-steps (:step wizard) @*state]] 
       [:div.block.has-text-centered
        (case (:step wizard)
          1 [:div [:p "Please choose jikaze"] [wizard-wind-keyboard theme :jikaze]]
          2 [:div [:p "Please choose bakaze"] [wizard-wind-keyboard theme :bakaze]]
          3 [:div [:p "Please choose dorahyouji"] [dorahyouji-keyboard theme hand] [dorahyouji-widget hand]]
          4 [:div [:p "Please enter closed hand"] [wizard-closed-hand-keyboard theme] [hand-render]]
          5 [:div [:p "Please enter open hand"] [wizard-open-hand-keyboard theme hand] [hand-render]]
          6 [:div [:p "Please enter agaripai"] [wizard-agaripai-keyboard theme hand] [agaripai-view (:agaripai hand)]]
          (swap! *state assoc-in [:wizard :step] 1))]
       [wizard-nav (:step wizard)]]]
     [:button.modal-close.is-large {:aria-label "close"
                                    :on-click #(swap! *state assoc-in [:wizard :open] false)}]]))

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
  [assoc-in (svg-tile theme tile) [1 :on-click]
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

(defn app-render []
  [:<>
   [hand-properties-render]
   [keyboard-render]
   [keyboard-mode-render]
   [hand-render]
   [settings-render]
   [wizard-render]])

(defn ^:export run []
  (play-tile-down-sfx)
  (rdom/render [app-render] (js/document.getElementById "interactive"))
  (rdom/render [results-render] (js/document.getElementById "results")))

(run)
