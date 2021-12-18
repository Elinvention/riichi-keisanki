(ns riichi-calc.tile
  (:gen-class))


(defn tile
  ([seed value red]
   (when (case seed
           (:man :sou :pin) (and (integer? value) (<= 1 value 9))
           :dragon (contains? #{:white :green :red} value)
           :wind (contains? #{:east :south :west :north} value)
           false)
     {:seed seed :value value :red red}))
  ([seed value] (tile seed value false)))
(defn man [value & {:keys [red] :or {red false}}] (tile :man value red))
(defn sou [value & {:keys [red] :or {red false}}] (tile :sou value red))
(defn pin [value & {:keys [red] :or {red false}}] (tile :pin value red))
(defn wind [value] (tile :wind value))
(defn dragon [value] (tile :dragon value))
(defn redfive [seed]
  (tile seed 5 true))

(defn pin? [{:keys [seed]}]
  (= :pin seed))
(defn sou? [{:keys [seed]}]
  (= :sou seed))
(defn man? [{:keys [seed]}]
  (= :man seed))

(defn numeral? [{:keys [seed value]}]
  (and (contains? #{:pin :sou :man} seed) (integer? value)))

(defn simple? [{:keys [value] :as tile}]
  (and (numeral? tile) (< 1 value 9)))

(defn terminal? [{:keys [value] :as tile}]
  (and (numeral? tile) (or (= value 1) (= value 9))))

(defn dragon? [{:keys [seed value]}]
  (and (= :dragon seed) (contains? #{:green :red :white} value)))

(defn wind? [{:keys [seed value]}]
  (and (= :wind seed) (contains? #{:east :west :north :south} value)))

(def honor? (some-fn wind? dragon?))

(def not-simple? (some-fn terminal? honor?))

(defn value? [{:keys [value] :as tile} wind-turn wind-seat]
  (or (dragon? tile)
      (and (wind? tile) (or (= wind-turn value) (= wind-seat value)))))

(defn five? [{:keys [value] :as tile}]
  (and (numeral? tile) (= 5 value)))

(defn redfive? [tile]
  (and (five? tile) (:red tile false)))



; Functions that operate on vector of tiles

(defn straight-int? [values]
  (if (not (every? int? values))
    (throw (IllegalArgumentException. "Only integers plz"))
    (every? #(= 1 %) (map - (rest values) values))))

(defn straight? [tiles]
  (and (= (count tiles) 3)
       (apply = (map :seed tiles))
       (every? numeral? tiles)
       (let [values (sort (map :value tiles))] (straight-int? values))))

(defn edge? [tiles]
  (and (straight? tiles) (some terminal? tiles) (apply distinct? tiles)))

(defn same? [& tiles]
  (and (apply = (map :seed tiles)) (apply = (map :value tiles))))

(defn count-tile [tile tiles]
  (count (filter (partial same? tile) tiles)))

(defn count-exact [tile tiles]
  (count (filter #{tile} tiles)))

(defn couple? [tiles] (and (= (count tiles) 2) (apply same? tiles)))
(defn tris? [tiles] (and (= (count tiles) 3) (apply same? tiles)))
(defn quad? [tiles] (and (= (count tiles) 4) (apply same? tiles)))

(defn tiles-group? [tiles]
  (or (couple? tiles)
      (tris? tiles)
      (quad? tiles)
      (straight? tiles)))

(defn repeat-tile [{:keys [seed value red]} n]
  (let [repeat-n (partial repeat n)]
    (some-> (tile seed value) (repeat-n) (vec) (assoc-in [0 :red] red))))
(defn couple [tile]
  (repeat-tile tile 2))
(defn tris [tile]
  (repeat-tile tile 3))
(defn quad [tile]
  (repeat-tile tile 4))
(defn straight [{:keys [seed value red]}]
  (when (and (contains? #{:man :sou :pin} seed)
             (integer? value) (<= 1 value 7))
    (mapv #(tile seed % (and red (= 5 %))) (range value (+ value 3)))))

(defn with-red [tiles index]
  (assoc-in tiles [index :red] true))

(defn five-index
  "Returns the index of the first tile with value = 5, nil if there are no 5s"
  [tiles]
  (let [indexed-tiles (map-indexed vector tiles)]
    (some #(when (five? (second %)) (first %)) indexed-tiles)))

(defn red-straight
  "Returns a straight with akadora if there is a five"
  [tile]
  (let [new-straight (straight tile)]
    (if-let [index (five-index new-straight)]
      (with-red new-straight index)
      new-straight)))

(defn taatsu?
  "Returns true if 2 tiles make a proto-run (like 45 or 46) else false"
  [[tile1 tile2 :as tiles]]
  (and (= 2 (count tiles))
       (apply = (map :seed tiles))
       (every? numeral? tiles)
       (< 0 (Math/abs (- (:value tile1) (:value tile2))) 3)))

(defn min-distance
  "Returns:
   - 0 if there is a tile in tiles
   - the minimum distance from other numerals if the tile is a numeral and
     there are same seed numerals in tiles
   - Integer/MAX_VALUE otherwise"
  [tiles tile]
  (cond
    (some #{tile} tiles) 0
    (numeral? tile) (if-let [numerals (not-empty (filter #(and (= (:seed %) (:seed tile)) (numeral? %)) tiles))]
                      (apply min (map #(Math/abs (- (:value %) (:value tile))) numerals))
                      Integer/MAX_VALUE)
    :else Integer/MAX_VALUE))


;; Tile sorting and naming

(defn tile-key
  "Maps each tile with an integer. Can be used with sort-by"
  [{:keys [value red] :as tile}]
  (+ (cond
       (man? tile) (* value 10)
       (sou? tile) (+ (* value 10) 100)
       (pin? tile) (+ (* value 10) 200)
       (wind? tile) (case value
                      :east 310
                      :south 320
                      :west 330
                      :north 340)
       (dragon? tile) (case value
                        :white 410
                        :green 420
                        :red 430))
     (if red 5 0)))

(defn sort-tiles [tiles]
  (into [] (sort-by tile-key tiles)))

(defn wind-next [wind]
  (case wind
    :east :south
    :south :west
    :west :north
    :north :east))

(defn dragon-next [dragon]
  (case dragon
    :white :green
    :green :red
    :red :white))

(defn tile-next [{:keys [seed value] :as mtile}]
  (cond
    (numeral? mtile) (tile seed (inc (mod value 9)))
    (dragon? mtile) (tile seed (dragon-next value))
    (wind? mtile) (tile seed (wind-next value))))

(defn wind-name [wind]
  (case wind
    :east  "Ton"
    :south "Nan"
    :west  "Shaa"
    :north "Pei"))

(defn dragon-name [dragon]
  (case dragon
    :white "Haku"
    :green "Hatsu"
    :red   "Chun"))

(defn tile-name [{:keys [seed value red]}]
  (case seed
    :man (str "Man" value (when red "-Dora"))
    :sou (str "Sou" value (when red "-Dora"))
    :pin (str "Pin" value (when red "-Dora"))
    :wind (wind-name value)
    :dragon (dragon-name value)))

(defn url-from-name [name]
  (str "file:resources/tiles/Export/Regular/" name ".png"))

(defn url [tile]
  (url-from-name (tile-name tile)))

(def all-34-tiles
  (vec
   (concat
    (for [seed [:man :sou :pin] value (range 1 10)]
      (tile seed value))
    (for [seed [:wind] value [:east :south :west :north]]
      (tile seed value))
    (for [seed [:dragon] value [:white :green :red]]
      (tile seed value)))))

(def all-37-tiles
  (vec
   (flatten (for [tile all-34-tiles]
              (if (= 5 (:value tile))
                [tile (redfive (:seed tile))]
                tile)))))
