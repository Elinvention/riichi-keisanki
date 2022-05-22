(ns riichi-calc.group
  (:require [riichi-calc.tile :as tile]))


(defrecord Group [tiles kind seed value red])

(defn group 
  ([[{:keys [seed value]} & _ :as tiles]]
   (let [red (some? (some tile/redfive? tiles))]
     (cond
       (tile/couple? tiles) (group tiles :couple seed value red)
       (tile/tris? tiles) (group tiles :tris seed value red)
       (tile/quad? tiles) (group tiles :quad seed value red)
       (tile/straight? tiles) (let [value (first (sort (map :value tiles)))]
                                (group tiles :straight seed value red))
       (tile/taatsu? tiles) (group tiles :taatsu seed value red)
       :else nil)))
  ([tiles kind seed value red]
   (when (and (contains? #{:couple :tris :straight :quad :taatsu} kind)
              (case seed
                (:man :sou :pin) (and (integer? value) (<= 1 value 9))
                :dragon (and (not= kind :straight) (contains? #{:white :green :red} value))
                :wind (and (not= kind :straight) (contains? #{:east :south :west :north} value))
                false))
     (Group. (vec tiles) kind seed value red))))

(defn group? [group]
  (contains? #{:couple :tris :quad :straight :taatsu} (:kind group)))

(defn expand [x]
  (if (group? x) (:tiles x) x))

(defn couple [tile]
  (group (tile/couple tile)))
(defn tris [tile]
  (group (tile/tris tile)))
(defn quad [tile]
  (group (tile/quad tile)))
(defn straight [tile]
  (group (tile/straight tile)))
(defn red-straight
  "Returns a group of straight tiles with akadora if there is a 5"
  [tile]
  (group (tile/red-straight tile)))

(defn straight? [group]
  (= (:kind group) :straight))
(defn tris? [group]
  (= (:kind group) :tris))
(defn quad? [group]
  (= (:kind group) :quad))
(defn couple? [group]
  (= (:kind group) :couple))
(defn taatsu? [group]
  (= (:kind group) :taatsu))

(defn to-string [group]
  (str "[" (apply str (map tile/tile-name (expand group))) "]"))

#?(:clj
   (defmethod print-method Group [group ^java.io.Writer w]
     (.write w (to-string group)))
   :cljs
   (extend-protocol IPrintWithWriter
     Group
     (-pr-writer [group w _]
       (write-all w (to-string group)))))

(defn non-couple? [group]
  ((some-fn straight? tris? quad?) group))

(defn man? [group]
  (= (:seed group) :man))
(defn sou? [group]
  (= (:seed group) :sou))
(defn pin? [group]
  (= (:seed group) :pin))

(defn simple? [{:keys [kind seed value]}]
  (cond
    (not (contains? #{:pin :sou :man} seed)) false
    (= kind :straight) (< 1 value 7)
    (contains? #{:couple :tris :quad :taatsu} kind) (< 1 value 9)
    :else false))

(defn numeral? [{:keys [seed]}]
  (contains? #{:man :sou :pin} seed))

(defn terminal? [{:keys [value kind] :as group}]
  (and (numeral? group) (not= kind :straight) (or (= value 1) (= value 9))))

(defn edge? [{:keys [kind value] :as group}]
  (and (numeral? group) (= kind :straight) (or (= value 1) (= value 7))))

(defn dragon? [{:keys [seed]}]
  (= :dragon seed))

(defn wind? [{:keys [seed]}]
  (= :wind seed))

(defn honor? [group]
  (or (wind? group) (dragon? group)))

(def not-simple? (some-fn terminal? honor?))

(def not-simple-edge? (some-fn not-simple? edge?))

(def terminal-edge? (some-fn terminal? edge?))

(defn value? [wind-turn wind-seat group]
  (tile/value? (first (expand group)) wind-turn wind-seat))

(defn redfive? [{:keys [value red]}]
  (and (= value 5) red))

(defn size [{:keys [kind]}]
  (case kind
    (:couple :taatsu) 2
    (:tris :straight) 3
    :quad 4))

(defn virtual-size [{:keys [kind]}]
 (case kind
   (:couple :taatsu) 2
   (:tris :straight :quad) 3))

(defn count-tile [group tile]
  (count (filter (partial tile/same? tile) (expand group))))

(defn count-doras [group dorahyouji]
  (apply + (map (partial count-tile group) (map tile/tile-next dorahyouji))))

(defn in? [tile group]
  (>= (.indexOf (expand group) tile) 0))

(defn fu [open group]
  (cond
    (tris? group) (if open
                    (if (simple? group) 2 4)
                    (if (simple? group) 4 8))
    (quad? group) (if open
                    (if (simple? group) 8 16)
                    (if (simple? group) 16 32))
    :else 0))

(defn group-key [{:keys [kind]}]
  (case kind
    :quad 1
    :tris 2
    :straight 3
    :couple 4
    :taatsu 5))

(defn neighbour? [tiles group]
  (some (partial tile/neighbour? tiles) (expand group)))
