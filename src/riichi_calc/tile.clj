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

(defn honor? [tile]
  ((some-fn wind? dragon?) tile))

(defn value? [{:keys [value] :as tile} wind-turn wind-seat]
  (or (dragon? tile)
      (and (wind? tile) (or (= wind-turn value) (= wind-seat value)))))

(defn redfive? [{:keys [value] :as tile}]
  (and (numeral? tile) (= 5 value) (:red tile false)))



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

(defn couple? [tiles] (and (= (count tiles) 2) (apply same? tiles)))
(defn tris? [tiles] (and (= (count tiles) 3) (apply same? tiles)))
(defn quad? [tiles] (and (= (count tiles) 4) (apply same? tiles)))

(defn tiles-group? [tiles]
  (or (couple? tiles)
      (tris? tiles)
      (quad? tiles)
      (straight? tiles)))

(defn repeat-tile [seed value n]
  (some->> (tile seed value) (repeat n) (vec)))
(defn couple [seed value] (repeat-tile seed value 2))
(defn tris [seed value] (repeat-tile seed value 3))
(defn quad [seed value] (repeat-tile seed value 4))
(defn straight [seed from]
  (when (and (contains? #{:man :sou :pin} seed)
           (integer? from) (<= 1 from 7))
    (mapv #(tile seed %) (range from (+ from 3)))))
(defn with-red [tiles index]
  (assoc-in tiles [index :red] true))

(defn tile-key [{:keys [value] :as tile}]
  (cond
    (man? tile) value
    (sou? tile) (+ value 10)
    (pin? tile) (+ value 20)
    (wind? tile) (case value
                   :east 31
                   :south 32
                   :west 33
                   :north 34)
    (dragon? tile) (case value
                     :white 41
                     :green 42
                     :red 43)))

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

(def all-37-tiles
  (concat
   (for [seed [:man :sou :pin] value (range 1 10)]
     (tile seed value))
   [(redfive :man) (redfive :sou) (redfive :pin)]
   (for [seed [:wind] value [:east :south :west :north]]
     (tile seed value))
   (for [seed [:dragon] value [:white :green :red]]
     (tile seed value))))
