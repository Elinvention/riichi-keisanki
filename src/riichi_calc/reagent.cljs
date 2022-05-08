(ns riichi-calc.reagent
  (:require [clojure.string :refer [capitalize]]
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
  [:image {:width 54 :xlinkHref (url-from-name theme "Front")}])

(defn back-tile [theme]
  [:image {:width 54 :xlinkHref (url-from-name theme "Back")}])

(defn svg-tile [theme tile rotated]
  [:svg {:xmlns "http://www.w3.org/2000/svg" :xmlnsXlink "http://www.w3.org/1999/xlink"
         :view-box "0 0 54 72"
         :width 54
         :transform (if rotated "rotate(90)" "")
         :style (if rotated {:margin "0 10px 0 10px"} {})}
   (if (some? tile)
     [:g [front-tile theme]
           [:image {:width 45 :xlinkHref (url theme tile) :transform "translate(4, 5)"}]]
     [back-tile theme])])

(defn vector-insert [v el pos]
  (vec (concat (subvec v 0 pos) [el] (subvec v pos))))

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
    (when (= (hand/space-left hand) 2)
      (swap! *state assoc :keyboard-mode :agaripai))))

(defn keyboard-key [layout tile]
  (assoc-in (svg-tile layout tile false)
            [1 :on-click]
            #(keyboard-input tile)))

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

(defn keyboard-render []
  (let [{:keys [keyboard-mode theme hand]} @*state]
    [:div (for [nine-tile (partition 9 9 nil (for [t tile/all-34-tiles]
                                               ^{:key (tile/tile-name t)} [keyboard-key theme t]))]
            ^{:key nine-tile} [:span {:style {:display :inline-block}} nine-tile])
     (radio-group "keyboard-mode"
                  [:an :chii :pon :kan :ankan :dorahyouji :agaripai]
                  keyboard-mode
                  #(swap! *state assoc :keyboard-mode %1)
                  "Keyboard mode:")
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
