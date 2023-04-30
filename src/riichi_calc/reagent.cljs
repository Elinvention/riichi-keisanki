(ns riichi-calc.reagent
  (:require [clojure.string :as s]
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
(defonce *theme (r/atom :regular))
(defonce *language (r/atom :romaji))
(defonce *wizard (r/atom {:step 1 :open false}))
(defonce *dragging (r/atom nil))
(defonce *dragend-new-state (r/atom state/initial-state))

(def tile-width 48)
(def tile-height 64)

(defn url-from-name [theme tname]
  (str "assets/tiles/" (s/capitalize (name theme)) "/" tname ".svg"))

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

(defn on-drag-start [tile event]
  (.setData (. event -dataTransfer) "application/riichi" (pr-str tile))
  (set! (.. event -dataTransfer -effectAllowed) "move")
  (println "dragging tile" (pr-str tile))
  (reset! *dragging tile)
  (reset! *dragend-new-state @*state))

(defn on-drag-end [_]
  (println "drag end")
  (reset! *dragging nil)
  (reset! *state @*dragend-new-state))

(defn svg-tile-container [svg-content]
  [:div.mahjong-tile {}
   [:svg {:xmlns "http://www.w3.org/2000/svg"
                       :xmlnsXlink "http://www.w3.org/1999/xlink"
                       :view-box (str "0 0 " tile-width " " tile-height)}
    svg-content]])

(defn svg-tile [theme tile]
  (svg-tile-container (if (some? tile)
                        (front-tile theme tile)
                        (back-tile-bg theme))))

(defn rotate-svg [tile]
  (assoc-in tile [1 :class] "rotated"))

(defn svg-tile-rotated [theme tile]
  (rotate-svg (svg-tile theme tile)))

(defn svg-tile-fg [theme tile]
  (svg-tile-container (front-tile-fg theme tile)))

(defn remove-from-hand [path index]
  (swap! *state update :hand hand/remove-from-hand path index))

(defn play-tile-down-sfx []
  (let [domaudio (js/document.getElementById "klick4")]
    (set! (.-currentTime domaudio) 0)
    (-> (.play domaudio)
        (.catch #(println "Can't play sound:" (. % -message))))))

(defn keyboard-key [update-state-fn theme tile enabled]
  (let [plain-tile (svg-tile theme tile)]
    (if enabled
      (assoc-in plain-tile [1] {:on-click #(swap! *state update-state-fn tile)
                                :draggable true
                                :on-drag-start (partial on-drag-start tile)
                                :on-drag-end on-drag-end})
      (update-in plain-tile [1 :style] assoc :opacity "50%"))))

(defn radio-group [options value on-change]
  [:<>
    (for [option options]
     ^{:key option} [:label.radio
                     [:input {:type :radio
                              :name (name option)
                              :checked (= option value)
                              :on-change #(on-change option)}]
                     (s/capitalize (name option))])])

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
      " " (s/capitalize bname)])])

(defn settings-render []
  [:div#settings
   [:fieldset.field [:legend "Theme"]
    [radio-group [:regular :black] @*theme (partial reset! *theme)]]
   [:fieldset.field [:legend "Yaku Names Language"]
    [radio-group [:ja :romaji :it :en] @*language (partial reset! *language)]]])

(defn on-drop [event]
  (.preventDefault event)
  (let [basket (keyword (s/lower-case (.. event -target -innerText)))
        tile @*dragging]
    (println (name basket) " received " tile) 
    (reset! *dragend-new-state
           (case basket
             :an (state/an-conj @*state tile)
             :chii (state/chii-conj @*state tile)
             :pon (state/pon-conj @*state tile)
             :kan (state/kan-conj @*state tile)
             :ankan (state/ankan-conj @*state tile)
             (do (println "Unknown basket") @*state)))))

(defn on-drag-over [event]
  (.preventDefault event)
  (set! (.. event -dataTransfer -dropEffect) "move"))

(defn keyboard-baskets-component []
  (let [dnd {:on-drop on-drop :on-drag-over on-drag-over}
        {:keys [hand]} @*state
        dragging @*dragging]
    [:div.columns.is-5
     (when (or (nil? dragging) (hand/can-add-tile? hand dragging))
       [:div.column [:p.notification.is-info dnd "An"]])
     (when (or (nil? dragging) (hand/can-add-chii? hand dragging))
       [:div.column [:p.notification.is-primary dnd "Chii"]])
     (when (or (nil? dragging) (hand/can-add-pon? hand dragging))
       [:div.column [:p.notification.is-warning dnd "Pon"]])
     (when (or (nil? dragging) (hand/can-add-kan? hand dragging))
       [:div.column [:p.notification.is-danger dnd "Kan"]])
     (when (or (nil? dragging) (hand/can-add-kan? hand dragging))
       [:div.column [:p.notification.is-link dnd "Ankan"]])]))

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
  (let [{:keys [keyboard-mode hand]} @*state 
        enabled? (partial state/can-input? keyboard-mode hand)]
    [keyboard-widget @*theme tile/all-34-tiles-with-redfives enabled? state/keyboard-input]))

(defn hand-tile [tile path pos svg-tile-fn dora]
  (cond-> (svg-tile-fn @*theme tile)
    true (assoc-in [1 :on-click] #(remove-from-hand path pos))
    dora (assoc-in [1 :class] "dora")))

(defn agaripai-view [tile]
  [:div.tile-button [:div "Agaripai"]
   (assoc-in (svg-tile @*theme tile) [1 :on-click]
             #(swap! *state assoc-in [:hand :agaripai] nil))])

(defn dorahyouji-tile [tile index]
  (cond-> (svg-tile @*theme tile)
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
  [:div.tile-button [:div (s/capitalize (name kind))]
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
  (for [[i group] (map-indexed vector min)
        [j tile] (map-indexed vector (group/expand group))]
    ^{:key (str "min" tile i j)}
    [hand-tile tile :min i (if (= j 0) svg-tile-rotated svg-tile) (hand/dora? hand tile)]))

(defn hand-render []
  (let [{:keys [hand]} @*state]
    [:div.hand (concat (hand-an-render hand) (hand-min-render hand))]))

(defn wizard-close! []
  (swap! *wizard assoc :open false))

(defn wizard-open! []
  (swap! *wizard assoc :open true))

(defn wizard-toggle! []
  (swap! *wizard update :open not))

(defn wizard-step-prev! []
  (swap! *wizard update :step (comp (partial max 1) dec)))

(defn wizard-step-next! []
  (swap! *wizard update :step (comp (partial min 6) inc)))

(defn buttons []
  [:div.field
   [:button.button.is-primary.mr-1 {:on-click wizard-open!} "Wizard"]
   [:button.button.is-danger {:on-click #(reset! *state state/initial-state)} "Reset"]])

(defn hand-properties-render []
  (let [{:keys [hand]} @*state theme @*theme language @*language]
    [:<>
     [buttons]
     [:div#hand-properties.field
      (wind-button (tile/wind (:bakaze hand)) :bakaze theme)
      (wind-button (tile/wind (:jikaze hand)) :jikaze theme)
      (agaripai-view (:agaripai hand))
      (dorahyouji-widget hand)]
     [agari-widget (:agari hand)]
     [extra-yaku-widget (:extra-yaku hand) language]]))



(defn wizard-steps [current-step {:keys [hand]}]
  [:ul.steps.has-content-centered.is-horizontal
   (let [steps [{:icon (svg-tile-fg @*theme (tile/wind (:jikaze hand))) :title "Jikaze"}
                {:icon (svg-tile-fg @*theme (tile/wind (:bakaze hand))) :title "Bakaze"}
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

(defn wizard-agaripai-keyboard [theme hand]
  [keyboard-widget theme tile/all-34-tiles-with-redfives (partial hand/can-agaripai? hand) state/set-agaripai])

(defn wizard-open-hand-keyboard [theme hand]
  [keyboard-widget theme tile/all-34-tiles-with-redfives (partial hand/can-add-pon? hand) state/pon-conj])

(defn wizard-closed-hand-keyboard [theme hand]
  [keyboard-widget theme tile/all-34-tiles-with-redfives (partial hand/can-add-tile? hand) state/an-conj])

(defn dorahyouji-keyboard [theme hand]
  [keyboard-widget theme tile/all-34-tiles-with-redfives (partial hand/can-add-dorahyouji? hand) state/dorahyouji-conj])

(defn wizard-render []
  (let [{:keys [hand]} @*state {:keys [open step]} @*wizard theme @*theme]
    [(if open :div#wizard.modal.is-active :div#wizard.modal)
     [:div.modal-background {:on-click wizard-close!}]
     [:div.modal-content
      [:div.card
       [:div.card-header [:p.card-header-title "Wizard"]]
       [:div.card-content
        [wizard-steps step @*state]] 
       [:div.block.has-text-centered
        (case step
          1 [:div [:p "Please choose jikaze"] [wizard-wind-keyboard theme :jikaze]]
          2 [:div [:p "Please choose bakaze"] [wizard-wind-keyboard theme :bakaze]]
          3 [:div [:p "Please choose dorahyouji"] [dorahyouji-keyboard theme hand] [dorahyouji-widget hand]]
          4 [:div [:p "Please enter closed hand"] [wizard-closed-hand-keyboard theme] [hand-render]]
          5 [:div [:p "Please enter open hand"] [wizard-open-hand-keyboard theme hand] [hand-render]]
          6 [:div [:p "Please enter agaripai"] [wizard-agaripai-keyboard theme hand] [agaripai-view (:agaripai hand)]]
          (swap! *state assoc-in [:wizard :step] 1))]
       [wizard-nav step]]]
     [:button.modal-close.is-large {:aria-label "close"
                                    :on-click wizard-close!}]]))

(defn result-win [lang {:keys [yakus han fu score]}]
  [:table [:thead [:tr [:th "Yaku Name"] [:th "Han Value"]]]
   [:tbody
    (for [yaku yakus
          :let [wiki (get-in yakudb [(key yaku) :wiki])
                name (get-in yakudb [(key yaku) :name lang] (s/capitalize (name (key yaku))))]]
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
  (let [{:keys [hand]} @*state theme @*theme language @*language
        {:keys [summary] :as res} (hand/results hand language)]
    (case (:type res)
      (:incomplete :invalid :agaripai :no-yaku) [:p summary]
      :tenpai (result-tenpai theme res)
      :winning (result-win language res))))

(defn new-key-prototype []
  (let [long-press (r/atom {:long false})]
   (fn [] 
     (let [plain-tile (svg-tile :regular (tile/wind :east))
           enabled true
           tile (-> plain-tile
                    (assoc-in [1 :on-mouse-down]
                              (fn []
                                (println "mouse-down")
                                (swap! long-press assoc :long false :start (js/Date.now))
                                (->> (js/setTimeout (fn [] 
                                                      (println "Timer fired!")
                                                      (let [elapsed (- (js/Date.now) (:start @long-press))]
                                                             (when (> elapsed 400)
                                                               (swap! long-press assoc :long true)
                                                               (println "Detected long press" @long-press elapsed))))
                                                    500)
                                     (swap! long-press assoc :timer))
                                (println @long-press)))
                    (assoc-in [1 :on-mouse-up]
                              (fn []
                                (println "clearTimeout")
                                (js/clearTimeout (:timer @long-press))))
                    )]
       (if enabled
         (if (:long @long-press)
           [:div
            [:div {:style {:background-color "red" :width "50px" :height "50px"}}]
            [:div {:style {:background-color "green" :width "50px" :height "50px"}}]
            [:div {:style {:background-color "blue" :width "50px" :height "50px"}}]
            tile]
           tile)
         (update-in plain-tile [1 :style] assoc :opacity "50%"))))))



(defonce *tile-chooser-state (r/atom []))

(def plus-tile (assoc-in
                (svg-tile-container
                 [:g (front-tile-bg @*theme)
                  [:image {:xlinkHref "assets/icons/plus.svg"
                           :width 40
                           :transform "translate(4, 12)"
                           :on-drag-start #(.preventDefault %)}]])
                [1 :on-click] #(swap! *tile-chooser-state conj true)))

(def back-tile (assoc-in
                (svg-tile-container
                 [:g (front-tile-bg @*theme)
                  [:image {:xlinkHref "assets/icons/back.svg"
                           :width 40
                           :transform "translate(4, 12)"
                           :on-drag-start #(.preventDefault %)}]])
                [1 :on-click] (fn [] (swap! *tile-chooser-state #(if (empty? %) % (pop %))))))

(def seeds [(tile/man 1) (tile/sou 1) (tile/pin 1) (tile/wind :east) (tile/dragon :red)])

(defn choose-seed-tile [theme tile]
  (assoc-in (svg-tile theme tile) [1 :on-click] #(swap! *tile-chooser-state conj (:seed tile))))

(defn choose-value-tile [theme tile]
  (assoc-in (svg-tile theme tile) [1 :on-click] #(swap! *tile-chooser-state conj (:value tile))))

(defn repeat-tile [n theme tile]
  (for [i (range n)]
    ^{:key i} [svg-tile theme tile]))

(defn choose-call! [fn tile]
  (swap! *state fn tile))

(defn choose-pon [theme tile]
  (when (hand/can-add-pon? (:hand @*state) tile)
    [:div.pon {:on-click #(choose-call! state/pon-conj tile)}
     (repeat-tile 3 theme tile)]))

(defn choose-kan [theme tile]
  (when (hand/can-add-kan? (:hand @*state) tile)
    [:div.kan {:on-click #(choose-call! state/kan-conj tile)}
     (repeat-tile 4 theme tile)]))

(defn choose-chii [theme tile]
  (when-let [tiles (tile/straight tile)]
    (when (hand/can-add-chii? (:hand @*state) tile)
      [:div.chii {:on-click #(choose-call! state/chii-conj tile)}
       (for [[i t] (map-indexed vector tiles)]
         ^{:key i} [svg-tile theme t])])))

(defn choose-atama [theme tile]
  (when (hand/can-add-tile? (:hand @*state) tile 2)
    [:div.atama {:on-click #(choose-call! state/atama-conj tile)}
     (repeat-tile 2 theme tile)]))

(defn choose-ankou [theme tile]
  (when (hand/can-add-pon? (:hand @*state) tile)
    [:div.ankou {:on-click #(choose-call! state/ankou-conj tile)}
     (repeat-tile 3 theme tile)]))

(defn choose-anjun [theme tile]
  (when-let [tiles (tile/straight tile)]
    (when (hand/can-add-chii? (:hand @*state) tile)
      [:div.anjun {:on-click #(choose-call! state/anjun-conj tile)}
       (for [t tiles]
         ^{:key (pr-str t)} [svg-tile theme t])])))

(defn choose-ankan [theme tile]
  (when (hand/can-add-kan? (:hand @*state) tile)
    [:div.ankan {:on-click #(choose-call! state/ankan-conj tile)}
     (for [i (range 4)]
       ^{:key i} [svg-tile theme (when (< 0 i 3) tile)])]))

(defn choose-group [theme tile]
  [:<>
   [:div.group.choose
    [:div.open [choose-pon theme tile] [choose-chii theme tile] [choose-kan theme tile]]
    [:div.close [choose-atama theme tile] [choose-ankou theme tile] [choose-anjun theme tile] [choose-ankan theme tile]]]
   back-tile])

(defn tile-chooser [[_ seed value :as state] theme]
  (case (count state)
    0 (when (> (hand/space-left (:hand @*state)) 0) plus-tile)
    1 [:<> [:div.choose (for [t seeds] ^{:key (pr-str t)} [choose-seed-tile theme t])] back-tile]
    2 [:<> [:div.choose (for [t (get tile/by-seed seed)] ^{:key (pr-str t)} [choose-value-tile theme t])] back-tile]
    [choose-group theme (tile/tile seed value)]))

(defn new-input-prototype []
  (let [tc @*tile-chooser-state
        theme @*theme]
    (conj (hand-render) (tile-chooser tc theme))))


(defn app-render []
  [:<>
   ;[new-key-prototype]
   [new-input-prototype]
   [hand-properties-render]
   [keyboard-render]
   [keyboard-baskets-component]
   [keyboard-mode-render]
   [hand-render]
   [settings-render]
   [wizard-render]])

(defn ^:export run []
  (play-tile-down-sfx)
  (rdom/render [app-render] (js/document.getElementById "interactive"))
  (rdom/render [results-render] (js/document.getElementById "results")))

(run)
