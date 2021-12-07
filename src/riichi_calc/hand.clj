(ns riichi-calc.hand
  (:gen-class)
  (:require clojure.set
            [clojure.math.numeric-tower :as math]
            [clojure.core.match :refer [match]]
            [riichi-calc.group :as group]
            [riichi-calc.tile :as tile]))


(comment {:an []   ;; from 暗 concealed tiles
          :min []  ;; from 明 open tiles
          :agari :ron   ;; from 和了り can be :ron or :tsumo
          :agaripai nil ;; from 和了り牌
          :bakaze :east ;; from 場風 
          :jikaze :east ;; from 自風
          :dorahyouji [] ;; from ドラ表示
          :riichi false ;; from 立直
          })

(defn hand [& {:keys [an min agari agaripai bakaze jikaze dorahyouji riichi ippatsu]
               :or {an [], min [], agari :tsumo, bakaze :east, jikaze :east,
                    dorahyouji [] riichi false ippatsu false}}]
  {:an an :min min :agari agari :agaripai agaripai :bakaze bakaze :jikaze jikaze
   :dorahyouji dorahyouji :riichi riichi :ippatsu ippatsu})

(defn closed? [{:keys [min]}]
  (empty? min))

(defn full [{:keys [an min]}]
  (concat an min))

(defn groups [hand]
  (filter :kind (full hand)))

(defn tiles [hand]
  (remove :kind (full hand)))

(defn expand [hand]
  (apply concat (for [tg (full hand)]
                  (if (:kind tg) (group/expand tg) [tg]))))

(defn count-yakuhai [{:keys [bakaze jikaze] :as hand}]
  (let [value? (partial group/value? bakaze jikaze)
        non-couple-value? (every-pred group/non-couple? value?)]
    (count (filter non-couple-value? (full hand)))))

(defn count-redfive [hand]
  (count (filter group/redfive? (full hand))))

(defn count-doras [{:keys [dorahyouji] :as hand}]
  (apply + (map #(group/count-doras % dorahyouji) (full hand))))

(defn count-own-tile [tile hand]
  (tile/count-tile tile (expand hand)))

(defn count-tile [tile {:keys [dorahyouji] :as hand}]
  (+ (count-own-tile tile hand) (tile/count-tile tile dorahyouji)))

(defn space-left [{:keys [an min]}]
  (- 14 (apply + (count (remove :kind an))
               (map group/virtual-size (filter :kind (concat an min))))))

(defn can-add-tile? [tile hand]
  (and (> (space-left hand) 0) (< (count-tile tile hand) (if (:red tile) 1 4))))

(defn can-add-pon? [tile hand]
  (and (> (space-left hand) 2) (< (count-tile tile hand) (if (:red tile) 1 2))))

(defn can-add-chii? [group hand]
  (and (some? group)
       (> (space-left hand) 2)
       (let [tiles (group/expand group)]
         (every? #(< (count-tile % hand) (if (:red %) 1 4)) tiles))))

(defn can-add-kan? [tile hand]
  (and (> (space-left hand) 2) (= (count-tile tile hand) 0)))

(defn machi
  "From 待ち, wait. Returns :tanki (pair wait), :shanpon (dual pon wait)
   :penchan (edge wait), :ryanmen (side wait) or :kanchan (closed wait)"
  [{:keys [agaripai] :as hand}]
  (when-let [agari-group (first (filter (partial group/in? agaripai) (groups hand)))]
    (case (:kind agari-group)
      :couple :tanki
      :tris :shanpon
      :straight (match [(group/expand agari-group) (:value agaripai)]
                  [[agaripai _ _] 7] :penchan
                  [[agaripai _ _] _] :ryanmen
                  [[_ agaripai _] _] :kanchan
                  [[_ _ agaripai] 3] :penchan
                  [[_ _ agaripai] _] :ryanmen))))


; Yaku yeeeee
(defn chiitoitsu? [{:keys [an min]}]
  (and (empty? min) (= (count an) 7) (every? group/couple? an)))

(def kokushi-tiles #{(tile/man 1) (tile/man 9) (tile/sou 1) (tile/sou 9)
                    (tile/pin 1) (tile/pin 9) (tile/wind :east)
                    (tile/wind :south) (tile/wind :west) (tile/wind :north)
                    (tile/dragon :white) (tile/dragon :green) (tile/dragon :red)})

(defn kokushi-musou? [{:keys [an min]}]
  (and (empty? min) (= (count an) 14) (every? #(some #{%} an) kokushi-tiles)))

(defn regular? [{:keys [an min]}]
  (and (= (+ (count an) (count min)) 5)
       (= (count (filter group/couple? an)) 1)
       (= (count (filter group/non-couple? (concat an min))) 4)))

(defn valid? [hand]
  (or (regular? hand) (chiitoitsu? hand) (kokushi-musou? hand)))

(defn tanyao? [{:keys [an]}]
  (every? group/simple? an))

(defn chinroutou? [hand]
  (every? group/terminal? (full hand)))

(defn chantaiyao? [hand]
  (every? (some-fn group/terminal? group/edge? group/honor?) (full hand)))

(defn honroutou? [hand]
  (every? (some-fn group/terminal? group/honor?) (full hand)))

(defn junchan-taiyao? [hand]
  (every? (some-fn group/terminal? group/edge?) (full hand)))

(defn sanshoku-doujin? [hand]
  (let [straights (distinct (filter group/straight? (full hand)))
        values (frequencies (map :value straights))
        seeds (set (map :seed straights))]
    (boolean (and (some #(>= % 3) (vals values)) (= (count seeds) 3)))))

(defn full-flush? [hand]
  (let [fhand (full hand)]
    (or (every? group/pin? fhand)
        (every? group/sou? fhand)
        (every? group/man? fhand))))

(defn half-flush? [hand]
  (and (not (full-flush? hand))
       (let [fhand (full hand)]
         (or (every? (some-fn group/pin? group/honor?) fhand)
             (every? (some-fn group/sou? group/honor?) fhand)
             (every? (some-fn group/man? group/honor?) fhand)))))

(defn iipeikou? [{:keys [an min] :as hand}]
  (and (regular? hand) (empty? min)
       (let [sequences (filter group/straight? an)]
         (= 1 (- (count sequences)
                 (count (dedupe (map #(dissoc % :red) sequences))))))))

(defn ryanpeikou? [{:keys [an min] :as hand}]
  (and (regular? hand) (empty? min)
       (let [sequences (filter group/straight? an)]
         (= 2 (- (count sequences) (count (dedupe sequences)))))))

(defn pinfu? [{:keys [an min bakaze jikaze] :as hand}]
  (and (empty? min) (regular? hand) (= (machi hand) :ryanmen)
       (not (some (partial group/value? bakaze jikaze) an))
       (= 4 (count (filter group/straight? an)))))

(defn ittsu? [hand]
  (let [straights (distinct (filter group/straight? (full hand)))
        dominant-seed (first (keys (max-key val (frequencies (map :seed straights)))))
        candidate (filter (every-pred
                           #(contains? #{1 4 7} (:value %))
                           #(= dominant-seed (:seed %))) straights)
        values (set (map :value candidate))]
    (boolean (and (contains? values 1) (contains? values 4) (contains? values 7)))))

(defn toitoi? [hand]
  (= 4 (count (filter group/tris? (full hand)))))

(defn sanankou? [{:keys [an]}]
  (= 3 (count (filter (some-fn group/tris? group/quad?) an))))

(defn sanshoku-doukou?
  "The hand includes three groups of triplets with the same number. "
  [hand]
  (when-let [tris (not-empty (filter (some-fn group/tris? group/quad?) (full hand)))]
    (let [tris-by-value (group-by :value (distinct tris))
          dominant (val (apply max-key key tris-by-value))]
      (and (>= (count dominant) 3) (= (count (distinct (map :seed dominant))) 3)))))

(defn sankantsu?
  "Three kans are called for this hand. "
  [hand]
  (= 3 (count (filter group/quad? (full hand)))))

(defn shousangen?
  "The hand contains two sets of 3 dragon tiles and a pair of the third dragon tiles."
  [hand]
  (let [dragons (filter group/dragon? (full hand))]
    (and (= 3 (count dragons)) (some group/couple? dragons))))

(defn daisangen?
  "The hand possesses three groups (triplets or quads) of all the dragons."
  [hand]
  (let [dragons (filter (every-pred group/dragon? group/non-couple?) (full hand))]
    (= 3 (count dragons))))

(defn suuankou?
  "This hand is composed of four groups of closed triplets.
  When this hand has a shanpon pattern and the win is via ron, then it would
  not be counted as such;
  only as the lesser toitoi with sanankou."
  [{:keys [an min agari] :as hand}]
  (and (empty? min)
       (= 4 (count (filter (some-fn group/tris? group/quad?) an)))
       (or (not= (machi hand) :shanpon) (= agari :tsumo))))

(defn shousuushii?
  "This hand has three groups (triplets or quads) of the wind tiles plus a pair of the fourth kind."
  [hand]
  (let [winds (filter group/wind? (full hand))]
    (and (= 4 (count winds)) (some group/couple? winds))))

(defn daisuushii?
  "This hand has four groups (triplets or quads) of all four wind tiles."
  [hand]
  (let [winds (filter (every-pred group/wind? group/non-couple?) (full hand))]
    (= 4 (count winds))))

(defn tsuuiisou?
  "Every group of tiles are composed of honor tiles."
  [hand]
  (let [winds (filter group/honor? (full hand))]
    (= 4 (count winds))))

(defn chuuren-poutou?
  "A hand consisting of the tiles 1112345678999 in the same suit plus any one
   extra tile of the same suit. "
  [hand]
  (and (full-flush? hand)
       (let [f (frequencies (map :value (expand hand)))]
         (every? identity (map #(>= (get f %1 0) %2) (range 1 10) [3 1 1 1 1 1 1 1 3])))))

(defn suukantsu?
  "Any hand with four calls of kan."
  [hand]
  (= 4 (count (filter group/quad? (full hand)))))

(defn list-yakus [{:keys [agari riichi ippatsu] :as hand}]
  (let [n-yakuhai (count-yakuhai hand)
        n-redfive (count-redfive hand)
        n-dora (count-doras hand)]
    (cond-> {}
      (> n-yakuhai 0) (assoc :yakuhai n-yakuhai)
      (> n-redfive 0) (assoc :redfive n-redfive)
      (> n-dora 0) (assoc :dora n-dora)
      (closed? hand) (cond->
                      (= agari :tsumo) (assoc :menzen-tsumo 1)
                      (iipeikou? hand) (assoc :iipeikou 1)
                      (pinfu? hand) (assoc :pinfu 1)
                      (chiitoitsu? hand) (assoc :chiitoitsu 2)
                      (kokushi-musou? hand) (assoc :kokushi-musou :yakuman)
                      (suuankou? hand) (assoc :suuankou :yakuman))
      (daisangen? hand) (assoc :daisangen :yakuman)
      (shousuushii? hand) (assoc :shousuushii :yakuman)
      (daisuushii? hand) (assoc :daisuushii :yakuman)
      (tsuuiisou? hand) (assoc :tsuuiisou :yakuman)
      (chuuren-poutou? hand) (assoc :chuuren-poutou :yakuman)
      (suukantsu? hand) (assoc :suukantsu :yakuman)
      (full-flush? hand) (assoc :chinitsu (if (closed? hand) 6 5))
      (half-flush? hand) (assoc :honitsu (if (closed? hand) 3 2))
      (tanyao? hand) (assoc :tanyao 1)
      (chinroutou? hand) (assoc :chinroutou :yakuman)
      (chantaiyao? hand) (assoc :chantaiyao (if (closed? hand) 2 1))
      (sanshoku-doujin? hand) (assoc :sanshoku-doujin (if (closed? hand) 2 1))
      (ittsu? hand) (assoc :ittsu (if (closed? hand) 2 1))
      (toitoi? hand) (assoc :toitoi (if (closed? hand) 2 1))
      (sanankou? hand) (assoc :sanankou 2)
      (sanshoku-doukou? hand) (assoc :sanshoku-doukou 2)
      (sankantsu? hand) (assoc :sankantsu 2)
      (honroutou? hand) (assoc :honroutou 2)
      (shousangen? hand) (assoc :shousangen 2)
      (junchan-taiyao? hand) (assoc :junchan-taiyao 3)
      (ryanpeikou? hand) (assoc :ryanpeikou 3)
      riichi (assoc :riichi 1)
      ippatsu (assoc :ippatsu 1))))

(defn no-yaku? [yakus]
  (empty? (dissoc yakus :dora :redfive)))

(defn hans [yakus]
  (if (some #(= :yakuman %) (vals yakus))
    :yakuman
    (apply + (vals yakus))))

(defn round-up-to [to number]
  (* to (inc (quot (dec number) to))))

(defn round-nearest [near number]
  (* near (Math/round (double (/ number near)))))

(defn minipoints [{:keys [an min agari bakaze jikaze] :as hand}]
  (if (chiitoitsu? hand)
    25
    (round-up-to
     10
     (cond-> (apply + (concat 
                       (map (partial group/fu false) an)
                       (map (partial group/fu true) min) [20]))
       (= agari :tsumo) (+ 2)
       (and (= agari :ron) (closed? hand)) (+ 10)
       (some (every-pred group/couple?
                         (partial group/value? bakaze jikaze)) hand) (+ 2)
       (contains? #{:kanchan :penchan :tanki} (machi hand)) (+ 2)))))

(defn limit-hands [han]
  (case han
    5        2000
    (6 7)    3000
    (8 9 10) 4000
    (11 12)  6000
    :yakuman 8000
    (when (> han 12) 8000)))

(comment
  (defn ron-score [han fu]
    (case han
      1 (case fu
          30  [1500 1000]
          40  [2000 1300]
          50  [2400 1600]
          60  [2900 2000]
          70  [3400 2300]
          80  [3900 2600]
          90  [4400 2900]
          100 [4800 3200]
          110 [5300 3600])
      2 (case fu
          25 [2400 1600]
          30 [2900 2000]
          40 [3900 2600]
          50 [4800 3200]
          60 [5800 3900]
          70 [6800 4500]
          80 [7700 5200]
          90 [8700 5800]
          100 [9600 6400]
          110 [10600 7100])
      3 (case fu
          25 [4800 3200]
          30 [5800 3900]
          40 [7700 5200]
          50 [9600 6400]
          60 [11600 7700]
          (when (> fu 60) [12000 8000]))
      4 (case fu
          25 [9600 6400]
          30 [11600 7700]
          (when (> fu 30) [12000 8000]))
      [(* 6 (limit-hands han)) (* 4 (limit-hands han))]))

  (defn tsumo-score [han fu]
    (case han
      1 (case fu
          30 [1500 1100]
          40 [2100 1500]
          50 [2400 1600]
          60 [3000 2000]
          70 [3600 2400]
          80 [3900 2700]
          90 [4500 3100]
          100 [4800 3200]
          110 [5400 3600])
      2 (case fu
          20 [2100 1500]
          30 [3000 2000]
          40 [3900 2700]
          50 [4800 3200]
          60 [6000 4000]
          70 [6900 4700]
          80 [7800 5200]
          90 [8700 5900]
          100 [9600 6400]
          110 [10800 72000])
      3 (case fu
          20 [3900 2700]
          25 [4800 3200]
          30 [6000 4000]
          40 [7800 5200]
          50 [9600 6400]
          60 [11700 7900]
          (when (> fu 60) [12000 8000]))
      4 (case fu
          20 [7800 5200]
          25 [9600 6400]
          30 [11700 7900]
          (when (> fu 30) [12000 8000]))
      [(* 6 (limit-hands han)) (* 4 (limit-hands han))])))

(defn basic-points [han fu]
  (cond
    (keyword? han) (limit-hands han)
    (<= 1 han 4) (* fu (math/expt 2 (+ 2 han)))
    (> han 4) (limit-hands han)))

(defn non-dealer-tsumo [han fu]
  (let [basic (basic-points han fu)]
    [(round-up-to 100 (* 2 basic)) (round-up-to 100 basic)]))

(defn dealer-tsumo [han fu]
  (round-up-to 100 (* 2 (basic-points han fu))))

(defn dealer-ron [han fu]
  (round-up-to 100 (* 6 (basic-points han fu))))

(defn non-dealer-ron [han fu]
  (round-up-to 100 (* 4 (basic-points han fu))))

(comment
  (for [[han fu] [[1 30] [2 30] [3 30] [4 30] [4 50]]]
    [(basic-points han fu)
     (dealer-tsumo han fu)
     (non-dealer-tsumo han fu)
     ;;(tsumo-score han fu)
     (dealer-ron han fu)
     (non-dealer-ron han fu)
     ;;(ron-score han fu)
     ]))

(defn round-thousandth [score]
  (-> score (/ 1000) double Math/ceil int))

(defn adjusted-final-scores [final-scores]
  (let [correction (apply + final-scores)
        winner (.indexOf final-scores (apply max final-scores))]
    (update final-scores winner #(- % correction))))

(defn final-scores [[score1 score2 score3 score4 :as raw-scores]
                    start target oka? uma-a uma-b]
  (if (not= (apply + raw-scores) (* 4 start))
    (throw (IllegalArgumentException.
            (str "Scores should add up to " (* 4 start))))
    (let [oka-prize (if oka? (* 4 (- target start)) 0)
          oka-target (if oka? target start)]
      [(-> score1 (- oka-target) (+ oka-prize) round-thousandth (+ uma-a))
       (-> score2 (- oka-target) round-thousandth (+ uma-b))
       (-> score3 (- oka-target) round-thousandth (- uma-b))
       (-> score4 (- oka-target) round-thousandth (- uma-a))])))

(defn mahjsoul-rank [scores room rounds]
  (case room
    :bronze (if (= rounds :east)
              (mapv + scores [10 5 0 0])
              (mapv + scores [20 10 0 0]))
    :silver (if (= rounds :east)
              (mapv + scores [20 10 0 0])
              (mapv + scores [40 20 0 0]))
    :gold (if (= rounds :east)
            (mapv + scores [40 20 0 0])
            (mapv + scores [80 40 0 0]))
    :jade (if (= rounds :east) 
            (mapv + scores [55 30 0 0])
            (mapv + scores [110 55 0 0]))
    :throne (if (= rounds :east)
              (mapv + scores [60 30 0 0])
              (mapv + scores [120 60 0 0]))))

(defn hand-summary [hand]
  (cond
    (> (space-left hand) 0) {:invalid "Please enter hand tiles"}
    (nil? (:agaripai hand)) {:invalid "Please choose agaripai (winning tile)"}
    :else (let [grouped (update-in hand [:an] group/group-hand)]
            (if (not (valid? grouped))
              {:invalid "Invalid hand"}
              (let [yakus (list-yakus grouped)]
                (if (no-yaku? yakus)
                  {:invalid "No yaku!"}
                  (let [han (hans yakus)
                        fu (minipoints grouped)
                        score (match [(:jikaze hand) (:agari hand)]
                                [:east :ron] (dealer-ron han fu)
                                [_ :ron] (non-dealer-ron han fu)
                                [:east :tsumo] (dealer-tsumo han fu)
                                [_ :tsumo] (non-dealer-tsumo han fu))]
                    {:yakus yakus :han han :fu fu :score score})))))))

(defn to-string [{:keys [an min]}]
  (str "{:an " (apply str
                      (map tile/tile-name (remove :kind an))
                      (map group/to-string (filter :kind an)))
       " :min " (apply str (map group/to-string min))))

(comment
  {:start-points 25000 :target-points 30000
   :players [{:name "Elia" :points 25000 :score 0 :seat :east}
             {:name "Giangi" :points 25000 :score 0 :seat :west}
             {:name "Lorenzo" :points 25000 :score 0 :seat :south}
             {:name "Luca" :points 25000 :score 0 :seat :north}]
   :rounds :east :turns []
   :uma [20 10] :oka 20000})
