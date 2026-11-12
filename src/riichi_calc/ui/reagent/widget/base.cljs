(ns riichi-calc.ui.reagent.widget.base
  (:require
   [clojure.string :as s]
   [reagent.core :as r]
   [riichi-calc.group :as group]
   [riichi-calc.hand :as hand]
   [riichi-calc.tile :as tile]
   [riichi-calc.ui.reagent.svg :as svg]))

(defn radio-group [group-name options value on-change]
  [:div.radios
   (for [option options]
     ^{:key option} [:label.radio
                     [:input {:type :radio
                              :name group-name
                              :value (name option)
                              :checked (= option value)
                              :on-change (fn [_] (on-change option))}]
                     (s/capitalize (name option))])])

(defn checkboxes [boxes on-change]
  [:div.checkboxes
   (for [box boxes
         :let [bname (get (val box) :name)
               checked (get (val box) :checked)
               disabled (get (val box) :disabled)
               closure (fn [_] (on-change (key box) (not checked)))]]
     ^{:key (str bname (val box))}
     [:label.checkbox.mr-2
      [:input {:type :checkbox
               :name bname
               :value bname
               :checked checked
               :disabled disabled
               :on-change closure}]
      " " (s/capitalize bname)])])

(defrecord TileMetaData [tile on-click-fn path pos dora?])

(defn hand-tile [theme {:keys [tile on-click-fn path pos dora?]}]
  (cond-> (svg/tile theme tile)
    (fn? on-click-fn) (assoc-in [1 :on-click] (fn [e]
                                                (.stopPropagation e)
                                                (on-click-fn path pos)))
    dora? (assoc-in [1 :class] "dora")))

(defn hand-group [theme tiles-meta on-click-fn path pos]
  [:div.tile-group {:on-click (fn [e]
                                (.stopPropagation e)
                                (on-click-fn path pos))}
   (for [[i tile-meta] (map-indexed vector tiles-meta)]
     ^{:key (str "group" i theme tiles-meta)}
     [hand-tile theme tile-meta])])

(defn- ankan->meta [ankan-group pos dora?]
  (map
   (fn [[j tile]] (TileMetaData. (when (< 0 j 3) tile) nil :an pos dora?))
   (map-indexed vector (group/expand ankan-group))))

(defn- hand-an-render [theme hand on-click]
  (reduce
   (fn [val [i group-or-tile]]
     (if (group/group? group-or-tile)
       (let [group group-or-tile
             tiles (group/expand group)
             tiles-meta (ankan->meta group i (hand/dora? hand (first tiles)))]
         (conj val
               ^{:key (str "group" i theme tiles on-click)}
               [hand-group theme tiles-meta on-click :an i]))
       (let [tile group-or-tile
             tile-meta (TileMetaData. tile on-click :an i (hand/dora? hand tile))]
         (conj val
               ^{:key (str "an" tile i)}
               [hand-tile theme tile-meta]))))
   []
   (map-indexed vector (:an hand))))

(defn- hand-min-render [theme hand on-click]
  (for [[i group] (map-indexed vector (:min hand))
        :let [tiles (map
                     (fn [tile] (TileMetaData. tile nil :min i (hand/dora? hand tile)))
                     (group/expand group))]]
    ^{:key (str "min" tiles i)}
    [hand-group theme tiles on-click :min i]))

(defn hand-render
  ([theme hand]
   [hand-render theme hand nil])
  ([theme hand on-click]
   [:div.field.notification.hand
    (concat (hand-an-render theme hand on-click)
            (hand-min-render theme hand on-click))]))
