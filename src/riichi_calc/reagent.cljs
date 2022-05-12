(ns riichi-calc.reagent
  (:require [clojure.string :refer [capitalize]]
            [clojure.core.match :refer [match]]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [riichi-calc.tile :as tile]
            [riichi-calc.group :as group]
            [riichi-calc.hand :as hand]))


(def initial-state {:hand (hand/hand)
                    :keyboard-mode :an
                    :akadora false
                    :theme :regular})

(def *state (r/atom initial-state))

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
     [:g [front-tile theme]
           [:image {:xlinkHref (url theme tile)
                    :width 45
                    :transform "translate(4, 5)"
                    :on-drag-start #(.preventDefault %)}]]
     [back-tile theme])])

(defn remove-from-hand [path index]
  (swap! *state update-in [:hand path]
         #(into (subvec % 0 index) (subvec % (inc index)))))

(defn keyboard-input [tile]
  (let [{:keys [hand keyboard-mode akadora]} @*state]
    (case keyboard-mode
      :an (when (hand/can-add-tile? hand tile)
            (swap! *state update-in [:hand :an] tile/conj-sort-tile tile))
      :chii (if akadora
              (when (hand/can-add-red-chii? hand tile)
                (when-let [red-straight (group/red-straight tile)]
                  (swap! *state update-in [:hand :min] conj red-straight)))
              (when (hand/can-add-chii? hand tile)
                (when-let [straight (group/straight tile)]
                  (swap! *state update-in [:hand :min] conj straight))))
      :pon (when (hand/can-add-pon? hand tile)
             (swap! *state update-in [:hand :min] conj (group/tris tile)))
      :kan (when (hand/can-add-kan? hand tile)
             (swap! *state update-in [:hand :min] conj (group/quad tile)))
      :ankan (when (hand/can-add-kan? hand tile)
               (swap! *state update-in [:hand :an] conj (group/quad tile)))
      :dorahyouji (when (hand/can-add-dorahyouji? hand tile)
                    (swap! *state update-in [:hand :dorahyouji] conj tile))
      :agaripai (if (> (hand/space-left hand) 0)
                  (when (hand/can-add-tile? hand tile)
                    (swap! *state update-in [:hand :an] tile/conj-sort-tile tile)
                    (swap! *state assoc-in [:hand :agaripai] tile))
                  (when (some #{tile} (hand/expand hand))
                    (swap! *state assoc-in [:hand :agaripai] tile))))
    (let [space (hand/space-left (:hand @*state))
          agaripai (:agaripai hand)
          next-kmode (match [space agaripai keyboard-mode]
                       [1 nil _] :agaripai
                       [0 nil _] :agaripai
                       [(_ :guard #(< % 3)) _ (:or :chii :pon :kan :ankan)] :an
                       :else keyboard-mode)]
      (when (not= next-kmode keyboard-mode)
        (swap! *state assoc :keyboard-mode next-kmode)))))

(defn keyboard-key [layout tile enabled]
  (let [plain-tile (svg-tile layout tile false)]
    (if enabled
      (assoc-in plain-tile [1 :on-click] #(keyboard-input tile))
      (update-in plain-tile [1 :style] assoc :opacity "50%"))))

(defn radio-group [group-name options value on-change legend]
  [:fieldset [:legend legend]
   (for [option options]
     ^{:key option} [:span
                     [:input {:type :radio
                              :id (name option)
                              :name group-name
                              :checked (= option value)
                              :on-change #(on-change option)}]
                     [:label {:for (name option)} (capitalize (name option))]])])

(defn checkboxes [legend boxes checks]
  [:fieldset [:legend legend]
   (for [box boxes]
     ^{:key box} [:span
                  [:input {:type :checkbox
                           :name (name box)
                           :value (name box)
                           :id (name box)
                           :checked (box checks)
                           :on-change #(swap! *state update box not)}]
                  [:label {:for (name box)} (capitalize (name box))]])])

(defn can-input? [{:keys [keyboard-mode hand akadora]} tile ukeire]
  (case keyboard-mode
    :an (hand/can-add-tile? hand tile)
    :chii (if akadora
            (hand/can-add-red-chii? hand tile)
            (hand/can-add-chii? hand tile))
    :pon (hand/can-add-pon? hand tile)
    :kan (hand/can-add-kan? hand tile)
    :ankan (hand/can-add-kan? hand tile)
    :dorahyouji (hand/can-add-dorahyouji? hand tile)
    :agaripai (if (> (hand/space-left hand) 0)
                (if (not-empty ukeire)
                  (some (partial tile/same? tile) ukeire)
                  (hand/can-add-tile? hand tile))
                (some #{tile} (hand/expand-groups (:an hand))))))

(defn keyboard-render []
  (let [{:keys [keyboard-mode theme hand akadora] :as state} @*state
        ukeire (if (= (hand/space-left hand) 1) (hand/ukeire hand) [])
        key-tiles (for [t tile/all-34-tiles
                        :let [akat (if (and akadora (= 5 (:value t)))
                                     (assoc t :red true) t)
                              enabled (can-input? state akat ukeire)]]
                    ^{:key (str (url theme akat) enabled)}
                    [keyboard-key theme akat enabled])]
    [:div (for [nine-tile (partition 9 9 nil key-tiles)]
            ^{:key nine-tile} [:span {:style {:display :inline-block}} nine-tile])
     (radio-group "keyboard-mode"
                  [:an :chii :pon :kan :ankan :dorahyouji :agaripai]
                  keyboard-mode
                  #(swap! *state assoc :keyboard-mode %1)
                  "Keyboard mode:")
     (checkboxes "Akadora" [:akadora] @*state)
     (radio-group "agari"
                  [:tsumo :ron]
                  (:agari hand)
                  #(swap! *state assoc-in [:hand :agari] %1)
                  "Agari:")]))

(defn hand-tile [tile path pos rotated]
  [assoc-in (svg-tile (:theme @*state) tile rotated) [1 :on-click] #(remove-from-hand path pos)])

(defn agaripai-view [tile]
  [:div.tile-button "Agaripai" [:br]
   (assoc-in (svg-tile (:theme @*state) tile false) [1 :on-click]
             #(swap! *state assoc-in [:hand :agaripai] nil))])

(defn- advance-wind [wind]
  (swap! *state update-in [:hand wind] tile/wind-next))

(defn- wind-button [wind kind theme]
  [:div.tile-button (capitalize (name kind)) [:br]
   (assoc-in (svg-tile theme wind false) [1 :on-click] #(advance-wind kind))])

(defn- hand-an-render [an]
  (reduce
   (fn [val [i group-or-tile]]
     (if (group/group? group-or-tile)
       (concat val
               (for [[j tile] (map-indexed vector (group/expand group-or-tile))]
                 ^{:key (str "an" tile i j)} [hand-tile (when (< 0 j 3) tile) :an i false]))
       (conj val ^{:key (str "an" group-or-tile i)} [hand-tile group-or-tile :an i false])))
   []
   (map-indexed vector an)))

(defn- hand-min-render [min]
  (for [[index group] (map-indexed vector min)
        [i tile] (map-indexed vector (group/expand group))]
        ;;{:fx/type min-view :tile tile :index index :rotate (if (= i 0) 90 0) :theme theme}
    ^{:key (str "min" tile index i)} [hand-tile tile :min index (= i 0)]))

(defn hand-render []
  (let [{:keys [hand theme]} @*state]
    [:div
     [:p (str hand " " (count (:an hand)) "an")]
     [:div
      (wind-button (tile/wind (:bakaze hand)) :bakaze theme)
      (wind-button (tile/wind (:jikaze hand)) :jikaze theme)
      (agaripai-view (:agaripai hand))]
     (concat (hand-an-render (:an hand)) (hand-min-render (:min hand)))]))

(defn results-render []
  (let [res (hand/results (:hand @*state))]
    (case (:type res)
      (:incomplete :tenpai :invalid :agaripai :no-yaku) [:p (:summary res)]
      :winning [:section
                [:h4 "Yaku:"]
                [:table [:tr [:th "Name"] [:th "Value"]]
                 (for [yaku (:yakus res)]
                       ^{:key yaku}[:tr [:td (capitalize (name (key yaku)))] [:td (val yaku)]])
                 [:tr.total [:td "Total"] [:td (hand/string-of-han (:han res) (:fu res))]]
                 [:tr.score [:td "Score"] [:td (hand/string-of-score (:score res))]]]])))

(defn ^:export run []
  (rdom/render [keyboard-render] (js/document.getElementById "keyboard"))
  (rdom/render [hand-render] (js/document.getElementById "hand"))
  (rdom/render [results-render] (js/document.getElementById "results")))

(run)
