(ns riichi-calc.group
  (:gen-class)
  (:require clojure.set [riichi-calc.tile :as tile]))


(defn group 
  ([[{:keys [seed value]} & _ :as tiles]]
   (let [red (some? (some tile/redfive? tiles))]
     (cond
       (tile/couple? tiles) (group tiles :couple seed value red)
       (tile/tris? tiles) (group tiles :tris seed value red)
       (tile/quad? tiles) (group tiles :quad seed value red)
       (tile/straight? tiles) (let [value (first (sort (map :value tiles)))]
                                (group tiles :straight seed value red))
       :else nil)))
  ([tiles kind seed value red]
   (when (and (contains? #{:couple :tris :straight :quad} kind)
              (case seed
                (:man :sou :pin) (and (integer? value) (<= 1 value 9))
                :dragon (and (not= kind :straight) (contains? #{:white :green :red} value))
                :wind (and (not= kind :straight) (contains? #{:east :south :west :north} value))
                false))
     {:tiles (vec tiles) :kind kind :seed seed :value value :red red})))

(defn group? [group]
  (contains? #{:couple :tris :quad :straight} (:kind group)))

(defn expand [{:keys [tiles]}]
  tiles)

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

(defn to-string [group]
  (str "[" (apply str (map tile/tile-name (expand group))) "]"))

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
    (contains? #{:couple :tris :quad} kind) (< 1 value 9)
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

(defn value? [wind-turn wind-seat group]
  (tile/value? (first (expand group)) wind-turn wind-seat))

(defn redfive? [{:keys [value red]}]
  (and (= value 5) red))

(defn size [{:keys [kind]}]
  (case kind
    :couple 2
    (:tris :straight) 3
    :quad 4))

(defn virtual-size [{:keys [kind]}]
 (case kind
   :couple 2
   (:tris :straight :quad) 3))

(defn count-tile [group tile]
  (count (filter (partial tile/same? tile) (expand group))))

(defn count-doras [group dorahyouji]
  (apply + (map (partial count-tile group) (map tile/tile-next dorahyouji))))

(defn in? [tile group]
  (>= (.indexOf (expand group) tile) 0))

(defn seq-sub
  "Subtracts a sequence s2 from a sequence s1 where s2 is a subset of s1.
  For example (seq-sub [1 1 2 3] [1 2]) => (1 3)"
  [s1 s2]
  (let [f1 (frequencies s1)
        f2 (frequencies s2)
        m (merge-with - f1 f2)
        r (mapcat #(repeat (second %) (first %)) m)]
    r))

(defn group-backtrack
  ([hand]
   (let [tiles (vec (remove :kind hand))
         groups (vec (filter :kind hand))]
     (if-let [grouped (group-backtrack [] tiles false 0)]
       (vec (concat grouped groups))
       hand)))
  ([visited not-visited couple-found depth]
   (println "group-backtrack" (map to-string visited)
            (map tile/tile-name not-visited) couple-found depth)
   (letfn
    [(try-consecutive
       [n]
       (when (or (not couple-found) (> n 2))
         (let [taken (take n not-visited)]
           (when (= (count taken) n)
             (when-let [new-group (group taken)]
               (group-backtrack (conj visited new-group)
                                (vec (nthrest not-visited n))
                                (if (= n 2) true couple-found)
                                (inc depth)))))))
     (try-straight
       []
       (let [tiles-deduped (take 3 (dedupe not-visited))]
         (when-let [group-deduped (group tiles-deduped)]
           (group-backtrack (conj visited group-deduped)
                            (vec (seq-sub not-visited tiles-deduped))
                            couple-found (inc depth)))))]
     (if (empty? not-visited) visited
         (or (try-consecutive 4) (try-consecutive 3)
             (try-straight) (try-consecutive 2))))))

(defn group-chiitoitsu [hand]
  (let [groups (mapv group (partition 2 2 hand))]
    (when (every? couple? groups) groups)))

(defn group-hand [hand]
  (let [sorted-hand (tile/sort-tiles hand)]
    (or (group-chiitoitsu sorted-hand)
        (group-backtrack sorted-hand))))

(defn fu [open group]
  (cond
    (tris? group) (if open
                    (if (simple? group) 2 4)
                    (if (simple? group) 4 8))
    (quad? group) (if open
                    (if (simple? group) 8 16)
                    (if (simple? group) 16 32))
    :else 0))

