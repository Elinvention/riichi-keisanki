(ns riichi-calc.hand
  (:require [clojure.string :as s]
            [clojure.core.match :refer [match]]
            [riichi-calc.group :as group]
            [riichi-calc.tile :as tile]))


(defn hand
  [& {:keys [an min agari agaripai bakaze jikaze dorahyouji extra-yaku]
      :or {an [], min [], agari :tsumo, bakaze :east, jikaze :east
           dorahyouji [] extra-yaku #{}}}]
  {:an (tile/sort-tiles an)   ;; from 暗 concealed tiles
   :min (tile/sort-tiles min) ;; from 明 open tiles
   :agari agari               ;; from 和了り can be :ron or :tsumo
   :agaripai agaripai         ;; from 和了り牌
   :bakaze bakaze             ;; from 場風
   :jikaze jikaze             ;; from 自風
   :dorahyouji dorahyouji     ;; from ドラ表示
   :extra-yaku extra-yaku})

(defn to-string [{:keys [an min]}]
  (if (empty? min)
    (str "Closed hand: <" (apply pr-str an) ">")
    (str "Open hand: <" (apply pr-str an) "> + <" (apply pr-str min) ">")))

(defn closed? [{:keys [min]}]
  (empty? min))

(defn full [{:keys [an min]}]
  (concat an min))

(defn groups-only [tiles-and-groups]
  (vec (filter :kind tiles-and-groups)))

(defn tiles-only [tiles-and-groups]
  (vec (remove :kind tiles-and-groups)))

(defn split-tiles-groups [tiles-and-groups]
  (let [groups (groups-only tiles-and-groups)
        tiles (tiles-only tiles-and-groups)]
    {:tiles tiles :groups groups}))

(defn expand-groups [tiles-groups]
  (->> tiles-groups
       (map #(if (:kind %) (group/expand %) [%]))
       (apply concat)
       (vec)))

(defn expand [hand]
  (expand-groups (full hand)))

(defn add-yaku [hand yaku]
  (if (or (not= yaku :ippatsu) (contains? (:extra-yaku hand) :riichi))
    (update hand :extra-yaku conj yaku)
    hand))

(defn remove-yaku [hand yaku]
  (update hand :extra-yaku disj yaku (when (= yaku :riichi) :ippatsu)))

(defn count-yakuhai [{:keys [bakaze jikaze] :as hand}]
  (let [value? (partial group/value? bakaze jikaze)
        non-couple-value? (every-pred group/non-couple? value?)]
    (count (filter non-couple-value? (full hand)))))

(defn count-redfive [hand]
  (count (filter group/redfive? (full hand))))

(defn dora? [{:keys [dorahyouji]} tile]
  (->> dorahyouji
       (map tile/tile-next)
       (set)
       (some (partial tile/same? tile))
       (boolean)))

(defn count-doras [{:keys [dorahyouji] :as hand}]
  (apply + (map #(group/count-doras % dorahyouji) (full hand))))

(defn count-own-tile [tile hand]
  (tile/count-tile tile (expand hand)))

(defn count-tile [tile {:keys [dorahyouji] :as hand}]
  (+ (count-own-tile tile hand) (tile/count-tile tile dorahyouji)))

(defn space-left [hand]
  (- 14
     (count (tiles-only (full hand)))
     (apply + (map group/virtual-size (groups-only (full hand))))))

(defn count-pairs [groups]
  {:pre [(some? groups)]}
  (count (filter group/couple? groups)))

(defn count-distinct-not-simples [tiles]
  {:pre [(some? tiles)]}
  (count (distinct (filter tile/not-simple? tiles))))

(defn count-terminal-pairs [groups]
  {:pre [(some? groups)]}
  (count (filter (every-pred group/couple? group/not-simple?) groups)))

;; shanten basic 8 point rules:
;; 8 - 2 * groups - 1 * pairs - 1 * proto
;; Special cases:
;; No more than 5 blocks (take in groups then pairs then proto)
;; if pairs = 0 and blocks >= 5 then add 1
;; TODO: rename groups to blocks and update everything to reflect that blocks = groups + pairs

(defn regular-shanten
  "Returns the shanten number of a regular hand."
  [{:keys [tiles groups]}]
  {:pre [(every? #(not (:kind %)) tiles)
         (every? :kind groups)]}
  (let [blocks (take 5 (sort-by group/group-key groups))
        nblocks (count blocks)
        ngroups (count (remove (some-fn group/couple? group/taatsu?) blocks))
        npairs (count (filter group/couple? blocks))
        ntaatsu (count (filter group/taatsu? blocks))]
    (- 8 (* 2 ngroups) npairs ntaatsu (if (and (>= nblocks 5) (= 0 npairs)) -1 0))))

(defn chiitoitsu-shanten
  "Returns the shanten number of a 7 pairs hand."
  [{:keys [tiles groups]}]
  {:pre [(every? #(not (:kind %)) tiles)
         (every? :kind groups)]}
  (- 6 (count-pairs groups)))

(defn kokushi-shanten
  "Returns the shanten number of a 13 orphans hand."
  [{:keys [tiles groups]}]
  {:pre [(every? #(not (:kind %)) tiles)
         (every? :kind groups)]}
  (- 13
     (count-distinct-not-simples tiles)
     (* 2 (min 1 (count-terminal-pairs groups)))))

(defn shanten
  "Returns the shanten number of any grouped hand."
  [hand]
  (min
   (regular-shanten (split-tiles-groups (full hand)))
   (chiitoitsu-shanten (split-tiles-groups (:an hand)))
   (kokushi-shanten (split-tiles-groups (:an hand)))))

(defn tenpai?
  "Returns true if the hand is ready (waiting for the winning tile), else false."
  [hand]
  (and (= (space-left hand) 1) (= (shanten hand) 0)))

(defn can-add-tile? [hand {:keys [red] :as tile}]
  (and (> (space-left hand) 0)
       (< (count-tile tile hand) 4)
       (or (not red) (= 0 (tile/count-exact tile (expand hand))))))

(defn can-add-pon? [hand {:keys [red] :as tile}]
  (and (> (space-left hand) 2)
       (< (count-tile tile hand) 2)
       (or (not red) (= 0 (tile/count-exact tile (expand hand))))))

(defn can-add-chii? [hand tile]
  (and (> (space-left hand) 2)
       (let [tiles (tile/straight tile)]
         (every? (partial can-add-tile? hand) tiles))))

(defn can-add-red-chii? [hand tile]
  (and (> (space-left hand) 2)
       (let [tiles (tile/red-straight tile)]
         (every? (partial can-add-tile? hand) tiles))))

(defn can-add-kan? [hand tile]
  (and (> (space-left hand) 2) (= (count-tile tile hand) 0)))

(defn can-add-dorahyouji? [{:keys [dorahyouji extra-yaku] :as hand} {:keys [red] :as tile}]
  {:pre [(vector? dorahyouji) (set? extra-yaku)]}
  (and (< (count dorahyouji) (if (contains? extra-yaku :riichi) 10 5))
       (< (count-tile tile hand) 4)
       (or (not red) (= 0 (tile/count-exact tile dorahyouji)))))

(defn machi
  "From 待ち, wait. Returns one of:
   - :tanki (pair wait)
   - :shanpon (dual pon wait)
   - :penchan (edge wait)
   - :ryanmen (side wait)
   - :kanchan (closed wait)
   - nil (incomplete hand or wrong agaripai)"
  [{:keys [agaripai] :as hand}]
  {:pre [(some? hand) (some? (:an hand))]}
  (let [groups (groups-only (full hand))]
    (when-let [agari-group (some #(when (group/in? agaripai %) %) groups)]
      (match [(:kind agari-group) (group/expand agari-group) (:value agaripai)]
        [:couple _ _] :tanki
        [:tris _ _] :shanpon
        [:straight [agaripai _ _] 7] :penchan
        [:straight [agaripai _ _] 7] :penchan
        [:straight [agaripai _ _] _] :ryanmen
        [:straight [_ agaripai _] _] :kanchan
        [:straight [_ _ agaripai] 3] :penchan
        [:straight [_ _ agaripai] _] :ryanmen))))

(defn juusan-menmachi?
  [{:keys [agaripai an]}]
  (contains? (set (tiles-only an)) agaripai))

; Yaku yeeeee
(defn chiitoitsu?
  "This hand is composed of seven pairs."
  [{:keys [an min]}]
  (and (empty? min) (= (count an) 7) (every? group/couple? an)))

(defn kokushi-musou?
  "This hand has one of each of the 13 different terminal and honor tiles plus
   one extra terminal or honour tile."
  [{:keys [an min]}]
  (let [exp-an (expand-groups an)]
    (and (empty? min) (= (count exp-an) 14)
         (every? #(some #{%} exp-an) tile/kokushi-tiles)
         (some (every-pred group/couple? group/not-simple?) an))))

(defn kokushi-musou-juusan-menmachi?
  "This happens when you have all the tiles required by kokushi musou and the
   wait is composed of 13 tiles."
  [{:keys [an min] :as hand}]
  (let [exp-an (expand-groups an)]
    (and (empty? min) (= (count exp-an) 14) (juusan-menmachi? hand)
         (every? #(some #{%} exp-an) tile/kokushi-tiles)
         (some (every-pred group/couple? group/not-simple?) an))))

(defn regular?
  "Returns true if the hand is made of 4 groups and a pair."
  [{:keys [an min]}]
  (and (= (+ (count an) (count min)) 5)
       (= (count (filter group/couple? an)) 1)
       (= (count (filter group/non-couple? (concat an min))) 4)))

(defn valid?
  "Returns true if the hand is valid (a regular, chiitoitsu or kokushi hand)."
  [hand]
  (or (regular? hand) (chiitoitsu? hand) (kokushi-musou? hand)))

(defn tanyao?
  "A hand composed of only simple (numerals 2-8) tiles."
  [hand]
  (every? group/simple? (full hand)))

(defn chinroutou?
  "Every group of tiles is composed of terminal tiles."
  [hand]
  (every? group/terminal? (full hand)))

(defn chantaiyao?
  "All tile groups contain at least 1 terminal or honor."
  [hand]
  (and (every? group/not-simple-edge? (full hand))
       (some group/terminal? (full hand))
       (some group/edge? (full hand))
       (some group/honor? (full hand))))

(defn honroutou?
  "The hand is composed of nothing but all terminals and honors."
  [hand]
  (and (every? group/not-simple? (full hand))
       (some group/terminal? (full hand))
       (some group/honor? (full hand))))

(defn junchan-taiyao?
  "All sets contain at least one terminal."
  [hand]
  (every? group/terminal-edge? (full hand)))

(defn sanshoku-doujun?
  "Three sequences have the same number across the three different suits."
  [hand]
  (let [straights (distinct (filter group/straight? (full hand)))
        values (frequencies (map :value straights))
        seeds (set (map :seed straights))]
    (boolean (and (some #(>= % 3) (vals values)) (= (count seeds) 3)))))

(defn chinitsu?
  "This hand is composed entirely of tiles from only one of the three suits."
  [hand]
  (let [fhand (full hand)]
    (or (every? group/pin? fhand)
        (every? group/sou? fhand)
        (every? group/man? fhand))))

(defn honitsu?
  "This is a single suit hand mixed with some honor tiles."
  [hand]
  (let [fhand (full hand)]
    (and (some group/numeral? fhand)
         (some group/honor? fhand)
         (or (every? (some-fn group/pin? group/honor?) fhand)
             (every? (some-fn group/sou? group/honor?) fhand)
             (every? (some-fn group/man? group/honor?) fhand)))))

(defn iipeikou?
  "This hand includes two identical sequences."
  [{:keys [an min] :as hand}]
  (and (regular? hand) (empty? min)
       (let [sequences (filter group/straight? an)]
         (= 1 (- (count sequences)
                 (count (dedupe (map #(dissoc % :red) sequences))))))))

(defn ryanpeikou?
  "This hand has two sets of \"iipeikou\"."
  [{:keys [an min] :as hand}]
  (and (regular? hand) (empty? min)
       (let [sequences (filter group/straight? an)]
         (= 2 (- (count sequences) (count (dedupe sequences)))))))

(defn pinfu?
  "Typically known as \"all sequences\", this is a hand that does not gain fu
   based on composition, other than that of a closed ron."
  [{:keys [an min bakaze jikaze] :as hand}]
  (and (empty? min) (regular? hand) (= (machi hand) :ryanmen)
       (not (some (partial group/value? bakaze jikaze) an))
       (= 4 (count (filter group/straight? an)))))

(defn ittsu?
  "This hand has a complete sequence of 1 through 9 of a single suit."
  [hand]
  (let [straights (distinct (filter group/straight? (full hand)))
        dominant-seed (first (keys (max-key val (frequencies (map :seed straights)))))
        candidate (filter (every-pred
                           #(contains? #{1 4 7} (:value %))
                           #(= dominant-seed (:seed %))) straights)
        values (set (map :value candidate))]
    (boolean (and (contains? values 1) (contains? values 4) (contains? values 7)))))

(defn toitoi?
  "The entire hand is composed of triplets."
  [hand]
  (= 4 (count (filter group/tris? (full hand)))))

(defn sanankou?
  "The hand includes three groups of triplets (or closed quads) that have been
   formed without calling any tiles.
   The fourth group can be an open triplet or sequence."
  [{:keys [an]}]
  (= 3 (count (filter (some-fn group/tris? group/quad?) an))))

(defn sanshoku-doukou?
  "The hand includes three groups of triplets with the same number. "
  [hand]
  (when-let [tris (not-empty (filter (some-fn group/tris? group/quad?) (full hand)))]
    (let [tris-by-value (group-by :value (distinct tris))
          dominant (val (apply max-key #(count (val %)) tris-by-value))]
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
       (or (= agari :tsumo) (= (machi hand) :shanpon))))

(defn suuankou-tanki?
  "This hand is composed of four groups of closed triplets. It is awarded when
   hand is won by tsumo and the hand has a tanki wait."
  [{:keys [an min] :as hand}]
  (and (empty? min) (= (machi hand) :tanki)
       (= 4 (count (filter (some-fn group/tris? group/quad?) an)))))

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
  (let [winds (filter group/honor? (groups-only (full hand)))]
    (= 5 (count winds))))

(defn chuuren-poutou?
  "A hand consisting of the tiles 1112345678999 in the same suit plus any one
   extra tile of the same suit. "
  [hand]
  (and (chinitsu? hand)
       (let [f (frequencies (map :value (expand hand)))]
         (every? identity
                 (map #(>= (get f %1 0) %2) (range 1 10) [3 1 1 1 1 1 1 1 3])))))

(defn junsei-chuuren-poutou?
  "A hand consisting of the tiles 1112345678999 in the same suit plus any one
   extra tile of the same suit. "
  [{:keys [agaripai] :as hand}]
  (and (chinitsu? hand)
       (let [tiles (expand hand)
             [head tail] (split-with (partial not= agaripai) tiles)
             without-agaripai (concat head (rest tail))
             f (frequencies (map :value without-agaripai))]
         (every? identity
                 (map #(= (get f %1 0) %2) (range 1 10) [3 1 1 1 1 1 1 1 3])))))

(defn suukantsu?
  "Any hand with four calls of kan."
  [hand]
  (= 4 (count (filter group/quad? (full hand)))))

(defn extra-yaku? [yaku {:keys [extra-yaku]}]
  (get extra-yaku yaku false))

(defn ippatsu? [{:keys [extra-yaku]}]
  (and (some #{:riichi :double-riichi} extra-yaku) (extra-yaku :ippatsu)))

(def closed-yaku-han
  {:menzen-tsumo    {:fun (fn [{:keys [agari]}] (= agari :tsumo)) :han 1}
   :iipeikou        {:fun iipeikou? :han 1}
   :pinfu           {:fun pinfu? :han 1}
   :ippatsu         {:fun ippatsu? :han 1}
   :houtei-raoyui   {:fun (partial extra-yaku? :houtei-raoyui) :han 1}
   :haitei-raoyue   {:fun (partial extra-yaku? :haitei-raoyue) :han 1}
   :rinshan-kaihou  {:fun (partial extra-yaku? :rinshan-kaihou) :han 1}
   :chankan         {:fun (partial extra-yaku? :chankan) :han 1}
   :tanyao          {:fun tanyao? :han 1}
   :riichi          {:fun (partial extra-yaku? :riichi) :han 1}
   :chiitoitsu      {:fun chiitoitsu? :han 2}
   :chantaiyao      {:fun chantaiyao? :han 2}
   :sanshoku-doujun {:fun sanshoku-doujun? :han 2}
   :ittsu           {:fun ittsu? :han 2}
   :toitoi          {:fun toitoi? :han 2}
   :sanankou        {:fun sanankou? :han 2}
   :sanshoku-doukou {:fun sanshoku-doukou? :han 2}
   :sankantsu       {:fun sankantsu? :han 2}
   :honroutou       {:fun honroutou? :han 2}
   :shousangen      {:fun shousangen? :han 2}
   :double-riichi   {:fun (partial extra-yaku? :double-riichi) :han 2}
   :honitsu         {:fun honitsu? :han 3}
   :junchan-taiyao  {:fun junchan-taiyao? :han 3}
   :ryanpeikou      {:fun ryanpeikou? :han 3}
   :chinitsu        {:fun chinitsu? :han 6}
   :kokushi-musou   {:fun kokushi-musou? :han :yakuman}
   :kokushi-juusan  {:fun kokushi-musou-juusan-menmachi? :han :yakuman}
   :suuankou        {:fun suuankou? :han :yakuman}
   :suuankou-tanki  {:fun suuankou-tanki? :han :yakuman}
   :daisangen       {:fun daisangen? :han :yakuman}
   :shousuushii     {:fun shousuushii? :han :yakuman}
   :daisuushii      {:fun daisuushii? :han :yakuman}
   :tsuuiisou       {:fun tsuuiisou? :han :yakuman}
   :chuuren-poutou  {:fun chuuren-poutou? :han :yakuman}
   :junsei-chuuren  {:fun junsei-chuuren-poutou? :han :yakuman}
   :suukantsu       {:fun suukantsu? :han :yakuman}
   :chinroutou      {:fun chinroutou? :han :yakuman}})

(def opened-yaku-han
  {:houtei-raoyui   {:fun (partial extra-yaku? :houtei-raoyui) :han 1}
   :haitei-raoyue   {:fun (partial extra-yaku? :haitei-raoyue) :han 1}
   :rinshan-kaihou  {:fun (partial extra-yaku? :rinshan-kaihou) :han 1}
   :chankan         {:fun (partial extra-yaku? :chankan) :han 1}
   :tanyao          {:fun tanyao? :han 1}
   :chantaiyao      {:fun chantaiyao? :han 1}
   :sanshoku-doujun {:fun sanshoku-doujun? :han 1}
   :ittsu           {:fun ittsu? :han 1}
   :toitoi          {:fun toitoi? :han 2}
   :sanankou        {:fun sanankou? :han 2}
   :sanshoku-doukou {:fun sanshoku-doukou? :han 2}
   :sankantsu       {:fun sankantsu? :han 2}
   :honroutou       {:fun honroutou? :han 2}
   :shousangen      {:fun shousangen? :han 2}
   :honitsu         {:fun honitsu? :han 2}
   :junchan-taiyao  {:fun junchan-taiyao? :han 2}
   :chinitsu        {:fun chinitsu? :han 5}
   :daisangen       {:fun daisangen? :han :yakuman}
   :shousuushii     {:fun shousuushii? :han :yakuman}
   :daisuushii      {:fun daisuushii? :han :yakuman}
   :tsuuiisou       {:fun tsuuiisou? :han :yakuman}
   :suukantsu       {:fun suukantsu? :han :yakuman}
   :chinroutou      {:fun chinroutou? :han :yakuman}})

(defn list-yakus [hand]
  (let [n-yakuhai (count-yakuhai hand)
        n-redfive (count-redfive hand)
        n-dora (count-doras hand)
        yaku-han (if (closed? hand) closed-yaku-han opened-yaku-han)
        yakus (reduce-kv
               (fn [m k {:keys [fun han]}]
                 (cond-> m
                   (fun hand) (assoc k han))) {} yaku-han)]
    (cond-> yakus
      (> n-yakuhai 0) (assoc :yakuhai n-yakuhai)
      (> n-redfive 0) (assoc :akadora n-redfive)
      (> n-dora 0) (assoc :dora n-dora))))

(defn no-yaku? [yakus]
  (empty? (dissoc yakus :dora :akadora)))

(defn hans [yakus]
  (let [yakuman-count (count (filter #{:yakuman} (vals yakus)))]
    (if (> yakuman-count 0)
      {:yakuman yakuman-count}
      {:regular (apply + (vals yakus))})))

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

(defn limit-hands [{:keys [yakuman regular]}]
  (case regular
    5        2000
    (6 7)    3000
    (8 9 10) 4000
    (11 12)  6000
    nil      (* yakuman 8000)
    (when (> regular 12) 8000)))

(defn intpow [base exp]
  (loop [acc 1 exp exp]
    (if (zero? exp) acc
        (recur (* base acc) (dec exp)))))

(defn basic-points [{:keys [yakuman regular] :as han} fu]
  (cond
    (some? yakuman) (limit-hands han)
    (<= 1 regular 4) (* fu (intpow 2 (+ 2 regular)))
    (> regular 4) (limit-hands han)))

(defn non-dealer-tsumo [han fu]
  (let [basic (basic-points han fu)
        dealer-pay (round-up-to 100 (* 2 basic))
        non-dealer-pay (round-up-to 100 basic)]
    {:dealer-pay dealer-pay :non-dealer-pay non-dealer-pay}))

(defn dealer-tsumo [han fu]
  {:everyone-pay (round-up-to 100 (* 2 (basic-points han fu)))})

(defn dealer-ron [han fu]
  {:ron-pay (round-up-to 100 (* 6 (basic-points han fu)))})

(defn non-dealer-ron [han fu]
  {:ron-pay (round-up-to 100 (* 4 (basic-points han fu)))})

(defn score [{:keys [jikaze agari]} han fu]
  (match [jikaze agari]
    [:east   :ron] (dealer-ron han fu)
    [_       :ron] (non-dealer-ron han fu)
    [:east :tsumo] (dealer-tsumo han fu)
    [_     :tsumo] (non-dealer-tsumo han fu)))

(defn round-thousandth [score]
  (-> score (/ 1000) double Math/ceil int))

(defn adjusted-final-scores [final-scores]
  (let [correction (apply + final-scores)
        winner (.indexOf final-scores (apply max final-scores))]
    (update final-scores winner #(- % correction))))

(defn final-scores [[score1 score2 score3 score4 :as raw-scores]
                    start target oka? uma-a uma-b]
  (if (not= (apply + raw-scores) (* 4 start))
    (throw #?(:clj (IllegalArgumentException. (str "Scores should add up to " (* 4 start)))
              :cljs (js/Error. (str "Scores should add up to " (* 4 start)))))
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

(defn- seq-sub
  "Subtracts a sequence s2 from a sequence s1 where s2 is a subset of s1.
  For example (seq-sub [1 1 2 3] [1 2]) => (1 3)"
  [s1 s2]
  (let [f1 (frequencies s1)
        f2 (frequencies s2)
        m (merge-with - f1 f2)
        r (mapcat #(repeat (second %) (first %)) m)]
    r))

(def tiles-sub (comp tile/sort-tiles seq-sub))

(defn group-consecutive [n {:keys [visited not-visited]}]
  (let [taken (take n not-visited)]
    (when (= (count taken) n)
      (when-let [new-group (group/group taken)]
        {:visited (conj visited new-group)
         :not-visited (vec (nthrest not-visited n))}))))

(defn group-straight [{:keys [visited not-visited]}]
  (let [tiles-deduped (take 3 (dedupe not-visited))]
    (when (= (count tiles-deduped) 3)
      (when-let [group-deduped (group/group tiles-deduped)]
        {:visited (conj visited group-deduped)
         :not-visited (tiles-sub not-visited tiles-deduped)}))))

(defn group-skip [{:keys [visited not-visited]}]
  (when (and (not-empty not-visited) (not-any? #(= (first not-visited) %) visited))
    {:visited (conj visited (first not-visited))
     :not-visited (vec (rest not-visited))}))

(defn group-greedy [{:keys [visited not-visited] :as decomposition}]
  (if (empty? not-visited)
    visited
    (recur (or (group-consecutive 4 decomposition)
               (group-consecutive 3 decomposition)
               (group-straight decomposition)
               (group-consecutive 2 decomposition)
               (group-skip decomposition)))))

(defn ->decomposition [{:keys [tiles groups]}]
  {:visited groups :not-visited tiles})

(def objective-fn (comp regular-shanten split-tiles-groups))

(defn lower-evaluation [{:keys [visited not-visited] :as decomposition}]
  (+ 0 (count (remove :kind visited))))

(defn group-branch-n-bound [decomposition]
  ;;(println "group-branch-n-bound" (:not-visited decomposition))
  (loop [best (group-greedy decomposition)
         bound (objective-fn best)
         queue [decomposition]
         steps 0]
    (if (or (empty? queue) (= -1 bound))
      (do (println "BnB found solution in" steps "steps") best)
      (let [decomp (peek queue)
            decomp-upper (objective-fn (:visited decomp))
            branches [(group-consecutive 4 decomp)
                      (group-consecutive 3 decomp)
                      (group-straight decomp)
                      (group-consecutive 2 decomp)
                      (group-skip decomp)]
            cut (->> branches
                     (remove nil?)
                     (remove #(> (lower-evaluation %) bound)))]
        ;;(println steps decomp-upper bound (count cut))
        (if (< decomp-upper bound)
          (recur (:visited decomp) decomp-upper (apply conj (pop queue) cut) (inc steps))
          (recur best bound (apply conj (pop queue) cut) (inc steps)))))))

(defn grouped [hand]
  (->> hand
       (:an)
       (tile/sort-tiles)
       (split-tiles-groups)
       (->decomposition)
       (group-branch-n-bound)
       (assoc hand :an)))

(def grouped-hand (comp grouped hand))

(defn hand-with-tile [hand tile]
  (grouped (update hand :an (comp expand-groups conj) tile)))

(defn debug->> [f x]
  (println (map f x))
  x)

(defn kokushi-ukeire [hand]
  (->> tile/kokushi-tiles
       (filter (partial can-add-tile? hand))  ;; that can be added to the hand
       (debug->> identity)
       (map #(hash-map :tile % :hand (hand-with-tile hand %)))  ;; build a hand with tile
       (debug->> #(shanten (:hand %)))
       (filter #(= (shanten (:hand %)) -1))  ;; check it's shanten
       (debug->> :tile)
       (filter #(valid? (:hand %)))  ;; check if it's a valid hand
       (map :tile)  ;; get back the added tiles
       (set)))

(defn split-ukeire-candidates [{:keys [tiles groups]}]
  (let [to-expand? (some-fn group/taatsu? group/couple? (partial group/neighbour? tiles))]
    {:tiles (concat tiles (expand-groups (filter to-expand? groups)))
     :groups (remove to-expand? groups)}))

(defn ukeire-candidate-tiles [hand]
  (loop [{:keys [groups] :as tg} (split-tiles-groups (:an (grouped hand)))]
    (let [{new-tiles :tiles new-groups :groups} (split-ukeire-candidates tg)]
      (if (= (count new-groups) (count groups))
        new-tiles
        (recur {:tiles new-tiles :groups new-groups})))))

(defn regular-ukeire
  "Brute force ukeire: build a new hand with a tile added and recompute it's shanten.
   If the shanten is -1 that tile is a winning one.
   The hand must lack only 1 tile."
  [hand]
  (if (not= 1 (space-left hand))
    []
    (let [tiles (ukeire-candidate-tiles hand)]
      (->> tile/all-34-tiles-with-redfives
           (filter (partial tile/neighbour? tiles))  ;; for each tile near one in the hand
           (filter (partial can-add-tile? hand))  ;; that can be added to the hand
           (debug->> identity)
           (map #(hash-map :tile % :hand (hand-with-tile hand %)))  ;; build a hand with tile
           (debug->> #(shanten (:hand %)))
           (filter #(= (shanten (:hand %)) -1))  ;; check it's shanten
           (debug->> :tile)
           (filter #(valid? (:hand %)))  ;; check if it's a valid hand
           (map :tile)  ;; get back the added tiles
           (set)))))  ;; put them in a set

(defn shape [hand]
  (if (closed? hand)
    (let [rs (regular-shanten (split-tiles-groups (full hand)))
          cs (chiitoitsu-shanten (split-tiles-groups (:an hand)))
          ks (kokushi-shanten (split-tiles-groups (:an hand)))]
      (first (min-key second [:regular rs] [:chiitoitsu cs] [:kokushi ks])))
    :regular))

(defn ukeire [hand]
  (case (shape hand)
    :regular (regular-ukeire hand)
    :chiitoitsu (regular-ukeire hand)
    :kokushi (kokushi-ukeire hand)))

(defn string-of-score [{:keys [everyone-pay dealer-pay non-dealer-pay ron-pay]}]
  (cond
    (some? everyone-pay) (str everyone-pay "⨉3")
    (every? some? [dealer-pay non-dealer-pay]) (str dealer-pay "+" non-dealer-pay "⨉2")
    (some? ron-pay) (str ron-pay)))

(defn string-of-yakus [yakus]
  (let [yakumans (filter #(= :yakuman (val %)) yakus)]
    (s/join "\n" (map (fn [yaku]
                        (let [yname (s/capitalize (name (key yaku)))
                              yval (if (integer? (val yaku))
                                     (val yaku)
                                     (s/capitalize (name (val yaku))))]
                          (str "★ " yname ": " yval)))
                      (if (empty? yakumans) yakus yakumans)))))

(defn string-of-han [{:keys [yakuman regular]} fu]
  (cond
    (some? yakuman) (str (case yakuman 1 "", 2 "Double ", 3 "Triple ") "Yakuman")
    (some? regular) (str regular " han " fu " fu")))

(defn results [hand]
  (if (> (space-left hand) 1)
    {:type :incomplete
     :summary "Please enter at least 13 tiles"}
    (let [gh (grouped hand)]
      (cond
        (tenpai? gh) (let [uke (ukeire gh)]
                       {:type :tenpai
                        :summary (str "Tenpai! Please choose agaripai (winning tile).\nUkeire: "
                                      (s/join ", " (map tile/tile-name uke)))
                        :ukeire uke})
        (not (valid? gh)) (let [shan (shanten gh)]
                            {:type :invalid
                             :summary (str "Invalid hand. Shanten: " shan)
                             :shanten shan})
        (nil? (:agaripai gh)) {:type :agaripai
                               :summary "Please choose agaripai (winning tile)."}
        :else (let [yakus (list-yakus gh)]
                (if (no-yaku? yakus)
                  {:type :no-yaku :summary "No yaku!"}
                  (let [han (hans yakus)
                        fu (minipoints gh)
                        score (score gh han fu)]
                    {:type :winning
                     :summary (str "Winning hand!\nYakus:\n" (string-of-yakus yakus)
                                   "\nPoints: " (string-of-han han fu)
                                   "\nScore: " (string-of-score score))
                     :yakus yakus :han han :fu fu :score score :agari (:agari gh)})))))))
