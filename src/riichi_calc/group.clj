(ns riichi-calc.group
  (:gen-class)
  (:require clojure.set [riichi-calc.tile :as tile]))


(defn group 
  ([[{:keys [seed value]} & _ :as tiles]]
   (let [red (some? (some tile/redfive? tiles))]
     (cond
       (tile/couple? tiles) (group :couple seed value red)
       (tile/tris? tiles) (group :tris seed value red)
       (tile/quad? tiles) (group :quad seed value red)
       (tile/straight? tiles) (let [value (first (sort (map :value tiles)))]
                                (group :straight seed value red))
       :else nil)))
  ([kind seed value red]
   (when (and (contains? #{:couple :tris :straight :quad} kind)
            (case seed
              (:man :sou :pin) (and (integer? value) (<= 1 value 9))
              :dragon (and (not= kind :straight) (contains? #{:white :green :red} value))
              :wind (and (not= kind :straight) (contains? #{:east :south :west :north} value))
              false))
     {:kind kind :seed seed :value value :red red})))

(defn group? [group]
  (contains? #{:couple :tris :quad :straight} (:kind group)))

(defn expand [{:keys [kind seed value]}]
  (case kind
    :couple (tile/couple seed value)
    :tris (tile/tris seed value)
    :quad (tile/quad seed value)
    :straight (tile/straight seed value)
    nil))

(defn couple
  ([seed value red] (group :couple seed value red))
  ([seed value] (couple seed value false))
  ([{:keys [seed value red]}] (couple seed value red)))
(defn tris
  ([seed value red] (group :tris seed value red))
  ([seed value] (tris seed value false))
  ([{:keys [seed value red]}] (tris seed value red)))
(defn quad
  ([seed value] (group :quad seed value (= value 5)))
  ([{:keys [seed value]}] (quad seed value)))
(defn straight
  ([seed value red] (group :straight seed value red))
  ([seed value] (straight seed value false))
  ([{:keys [seed value red]}] (straight seed value red)))

(defn straight? [group]
  (= (:kind group) :straight))
(defn tris? [group]
  (= (:kind group) :tris))
(defn quad? [group]
  (= (:kind group) :quad))
(defn couple? [group]
  (= (:kind group) :couple))

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

(defn group-backtrack
  ([hand] (concat (group-backtrack [] (remove :kind hand) false) (filter :kind hand)))
  ([visited not-visited couple-found]
   (letfn
    [(try-extract
       [n]
       (when (or (not couple-found) (> n 2))
         (let [mgroup (group (take n not-visited))]
           (if (some? mgroup)
             (group-backtrack (conj visited mgroup)
                      (nthrest not-visited n)
                      (if (= n 2) true couple-found))
             (let [tiles-deduped (take n (dedupe not-visited))
                   group-deduped (group tiles-deduped)]
               (when (some? group-deduped)
                 (group-backtrack (concat visited (repeat (* 2 (count group-deduped)) group-deduped))
                          (filter #(some (set tiles-deduped) %) not-visited)
                          (if (= n 2) true couple-found))))))))]
     (if (empty? not-visited) visited
         (or (try-extract 2) (try-extract 3) (try-extract 4))))))

(defn group-chiitoitsu [hand]
  (let [groups (mapv group (partition 2 2 hand))]
    (when (every? couple? groups) groups)))

(defn group-hand [hand]
  (let [sorted-hand (sort-by tile/tile-key hand)]
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

