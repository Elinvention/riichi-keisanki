(ns riichi-calc.hand-test
  (:require [clojure.test :refer [deftest is are testing]]
            [riichi-calc.hand :as hand]
            [riichi-calc.group :as group]
            [riichi-calc.tile :as tile]))


(def kokushi-hand (hand/hand :an tile/kokushi-tiles :agaripai (tile/dragon :red)))
(def complete-kokushi (update kokushi-hand :an conj (tile/dragon :red)))
(def grouped-kokushi (hand/grouped complete-kokushi))

(deftest test-machi
  (testing "machi"
    (is (= #{:penchan} (hand/machi (hand/hand :an [(group/straight (tile/man 1))] :agaripai (tile/man 3)))))
    (is (= #{:ryanmen} (hand/machi (hand/hand :an [(group/straight (tile/man 3))] :agaripai (tile/man 3)))))
    (is (= #{:kanchan} (hand/machi (hand/hand :an [(group/straight (tile/man 3))] :agaripai (tile/man 4)))))
    (is (= #{:shanpon} (hand/machi (hand/hand :an [(group/tris (tile/man 3))] :agaripai (tile/man 3)))))
    (is (= #{:tanki} (hand/machi (hand/hand :an [(group/couple (tile/man 3))] :agaripai (tile/man 3)))))
    (is (= #{:penchan :ryanmen} (hand/machi (hand/hand :an [(group/straight (tile/pin 1))
                                                            (group/straight (tile/pin 3))]
                                                       :agaripai (tile/pin 3)))))))

(deftest yaku-predicates
  (testing "valid?"
    (is (not (hand/valid? (hand/hand))))
    (is (not (hand/valid? (hand/hand :an (repeat 5 (group/quad (tile/pin 1)))))))
    (is (not (hand/valid? (hand/hand :an (vec (repeat 5 (group/quad (tile/pin 1))))))))
    (is (hand/valid? (hand/hand :an (conj (mapv #(group/tris (tile/pin %)) [1 2 3 4]) (group/couple (tile/pin 9))))))
    (is (hand/valid? (hand/hand :an (repeat 7 (group/couple (tile/pin 1))))))
    (is (hand/valid? (hand/hand :an (mapv #(group/couple (tile/pin %)) (range 1 8)))))
    (is (hand/valid? grouped-kokushi)))
  (testing "sanshoku-doujin"
    (is (hand/sanshoku-doujun? (hand/hand :an [(group/straight (tile/pin 1)) (group/straight (tile/sou 1))
                                               (group/straight (tile/man 1))])))
    (is (hand/sanshoku-doujun? (hand/hand :an [(group/straight (tile/pin 1)) (group/straight (tile/pin 1))
                                               (group/straight (tile/man 1)) (group/straight (tile/sou 1))])))
    (is (not (hand/sanshoku-doujun? (hand/hand :an [(group/straight (tile/pin 1)) (group/straight (tile/sou 2))
                                                    (group/straight (tile/man 1)) (group/straight (tile/man 2))])))))
  (testing "full-flush"
    (is (hand/chinitsu? (hand/hand :an [(group/straight (tile/pin 1)) (group/straight (tile/pin 2))
                                        (group/straight (tile/pin 3)) (group/tris (tile/pin 9))
                                        (group/couple (tile/pin 5))])))
    (is (not (hand/chinitsu? (hand/hand :an [(group/straight (tile/sou 1)) (group/straight (tile/pin 1))
                                             (group/straight (tile/pin 1)) (group/tris (tile/pin 9))
                                             (group/couple (tile/pin 5))]))))
    (is (not (hand/chinitsu? (hand/hand :an [(group/tris (tile/dragon :green)) (group/quad (tile/dragon :red))
                                             (group/straight (tile/pin 1)) (group/tris (tile/pin 9))
                                             (group/couple (tile/pin 5))])))))
  (testing "half-flush"
    (is (not (hand/honitsu? (hand/hand :an [(group/tris (tile/pin 4)) (group/quad (tile/pin 6))
                                            (group/straight (tile/pin 1)) (group/tris (tile/pin 9))
                                            (group/couple (tile/pin 5))]))))
    (is (hand/honitsu? (hand/hand :an [(group/tris (tile/dragon :green)) (group/quad (tile/dragon :red))
                                       (group/straight (tile/pin 1)) (group/tris (tile/pin 9))
                                       (group/couple (tile/pin 5))]))))
  (testing "iipeikou"
    (is (hand/iipeikou? (hand/hand :an [(group/straight (tile/pin 1)) (group/straight (tile/pin 1))
                                        (group/straight (tile/pin 7)) (group/quad (tile/pin 2))
                                        (group/couple (tile/man 1))])))
    (is (not (hand/iipeikou? (hand/hand :an [(group/straight (tile/pin 1)) (group/straight (tile/pin 1))
                                             (group/straight (tile/pin 7)) (group/straight (tile/pin 7))
                                             (group/couple (tile/pin 5))]))))
    (is (hand/iipeikou? (hand/hand :an [(group/straight (tile/pin 1)) (group/straight (tile/pin 1))
                                        (group/straight (tile/sou 5)) (group/straight (tile/pin 7))
                                        (group/couple (tile/pin 5))]))))
  (testing "chuuren poutou"
    (is (hand/chuuren-poutou? (hand/hand :an [(group/tris (tile/man 1)) (group/straight (tile/man 1))
                                              (group/straight (tile/man 4)) (group/straight (tile/man 7))
                                              (group/couple (tile/man 9))]))))
  (testing "dora?"
    (is (not (hand/dora? (hand/hand) (tile/man 2))))
    (is (hand/dora? (hand/hand :dorahyouji [(tile/man 1)]) (tile/man 2)))))

(deftest counting
  (testing "count-doras"
    (let [h (hand/hand :an [(group/straight (tile/pin 1)) (group/straight (tile/pin 1))
                            (group/straight (tile/pin 7)) (group/tris (tile/pin 4))
                            (group/couple (tile/pin 5))]
                       :dorahyouji [(tile/pin 9) (tile/pin 6)])]
      (is (= 3 (hand/count-doras h)))))
  (testing "count redfives"
    (let [h (hand/grouped-hand :an [(group/red-straight (tile/pin 3))] :agaripai (tile/pin 3))]
      (is (= 1 (hand/count-redfive h)))))
  (testing "space left"
    (is (= 14 (hand/space-left (hand/hand))) "Empty hand => 14 tiles to go")
    (let [h (hand/grouped-hand :an [(tile/man 1)])]
      (is (= 1 (count (hand/expand h))))
      (is (= 13 (hand/space-left h))))
    (let [h (hand/grouped-hand :an (mapv
                                    #(apply tile/tile %)
                                    [[:man 1] [:man 1] [:man 2]
                                     [:sou 1] [:sou 2] [:sou 3]
                                     [:sou 4] [:sou 5] [:sou 6]
                                     [:sou 7] [:sou 8] [:sou 9]
                                     [:pin 1]]))]
      (is (= 13 (count (hand/expand h))))
      (is (= 1 (hand/space-left h))))
    (let [h (hand/grouped-hand :an (mapv
                                    #(apply tile/tile %)
                                    [[:sou 1] [:sou 2] [:sou 3]
                                     [:sou 4] [:sou 5] [:sou 6]
                                     [:sou 7] [:sou 8] [:sou 9]])
                               :min [(group/quad (tile/pin 1))])]
      (is (= 13 (count (hand/expand h))))
      (is (= 2 (hand/space-left h))))))

(deftest list-yakus-test
  (testing "Yaku and Han computation"
    (let [h (hand/hand :an [(group/straight (tile/pin 1)) (group/straight (tile/pin 1))
                            (group/straight (tile/pin 7)) (group/straight (tile/pin 4))
                            (group/couple (tile/pin 5))]
                       :agari :tsumo, :bakaze :east, :jikaze :east
                       :agaripai (tile/pin 1), :dorahyouji [(tile/pin 9)])]
      (is (=
           {:dora 2, :menzen-tsumo 1, :iipeikou 1, :pinfu 1, :chinitsu 6 :ittsu 2}
           (hand/list-yakus h))))

    (let [h (hand/hand :an [(group/straight (tile/pin 1)) (group/straight (tile/pin 7))
                            (group/straight (tile/pin 4)) (group/couple (tile/pin 5))]
                       :min [(group/straight (tile/pin 1))]
                       :agari :tsumo, :bakaze :east, :jikaze :east
                       :agaripai (tile/pin 1), :dorahyouji [(tile/pin 9)])]
      (is (= {:dora 2, :chinitsu 5, :ittsu 1} (hand/list-yakus h))))

    (let [h (hand/hand :an [(group/tris (tile/wind :east)) (group/straight (tile/pin 7))
                            (group/straight (tile/pin 4)) (group/couple (tile/pin 5))]
                       :min [(group/tris (tile/dragon :white))]
                       :agari :tsumo, :bakaze :east, :jikaze :east
                       :agaripai (tile/pin 4), :dorahyouji [(tile/dragon :green)])]
      (is (= {:yakuhai 2, :honitsu 2} (hand/list-yakus h))))

    (is (= 1
           (:yakuman
            (let [h (hand/hand :an [(group/tris (tile/sou 9)) (group/tris (tile/pin 1))
                                    (group/tris (tile/man 1)) (group/couple (tile/sou 1))]
                               :min [(group/tris (tile/pin 9))]
                               :agari :tsumo, :bakaze :east, :jikaze :east
                               :dorahyouji [(tile/dragon :white)])]
              (hand/hans (hand/list-yakus h))))))

    (is (= (let [h (hand/hand :an [(group/straight (tile/pin 1)) (group/straight (tile/pin 4))
                                   (group/straight (tile/pin 7)) (group/straight (tile/man 1))
                                   (group/couple (tile/sou 1))]
                              :agari :ron, :bakaze :east, :jikaze :north
                              :dorahyouji [(tile/dragon :red)])]
             (hand/list-yakus h))
           {:ittsu 2}))
    (is (= (let [h (hand/hand :an [(group/tris (tile/man 1)) (group/straight (tile/man 2))
                                   (group/couple (tile/man 6)) (group/straight (tile/man 4))
                                   (group/straight (tile/sou 7))]
                              :agari :tsumo, :bakaze :east, :jikaze :north
                              :dorahyouji [(tile/sou 8)])]
             (hand/list-yakus h))
           {:dora 1, :menzen-tsumo 1}))))


(deftest minipoints-test
  (testing "minipoints for all"
    (is (= 50 (let [h (hand/hand :an [(group/tris (tile/dragon :white)) (group/tris (tile/wind :east))
                                      (group/straight (tile/pin 7)) (group/straight (tile/pin 4))
                                      (group/couple (tile/pin 5))]
                                 :agari :ron, :jikaze :west)]
                (hand/minipoints h))))

    (is (= 25 (let [t [(tile/pin 2) (tile/sou 3) (tile/man 4) (tile/pin 5)
                       (tile/sou 6) (tile/pin 7) (tile/pin 8)]
                    h (hand/hand :an (mapv group/couple t))]
                (hand/minipoints h))))))
(deftest scoring
  (testing "rounding"
    (is (= 13 (hand/round-thousandth 12500)))
    (is (= 13 (hand/round-thousandth 12400)))
    (is (= 13 (hand/round-thousandth 12600))))
  (testing "dealer ron"
    (is (= 1500 (:ron-pay (hand/dealer-ron {:regular 1} 30))))
    (is (= 2900 (:ron-pay (hand/dealer-ron {:regular 2} 30))))
    (is (= 5800 (:ron-pay (hand/dealer-ron {:regular 3} 30))))
    (is (= 11600 (:ron-pay (hand/dealer-ron {:regular 4} 30)))))
  (testing "non dealer ron"
    (is (= 1000 (:ron-pay (hand/non-dealer-ron {:regular 1} 30))))
    (is (= 2000 (:ron-pay (hand/non-dealer-ron {:regular 2} 30))))
    (is (= 3900 (:ron-pay (hand/non-dealer-ron {:regular 3} 30))))
    (is (= 7700 (:ron-pay (hand/non-dealer-ron {:regular 4} 30)))))
  (testing "dealer tsumo"
    (is (= 500 (:everyone-pay (hand/dealer-tsumo {:regular 1} 30))))
    (is (= 1000 (:everyone-pay (hand/dealer-tsumo {:regular 2} 30))))
    (is (= 2000 (:everyone-pay (hand/dealer-tsumo {:regular 3} 30))))
    (is (= 3900 (:everyone-pay (hand/dealer-tsumo {:regular 4} 30)))))
  (testing "non dealer tsumo"
    (is (= 500 (:dealer-pay (hand/non-dealer-tsumo {:regular 1} 30))))
    (is (= 1000 (:dealer-pay (hand/non-dealer-tsumo {:regular 2} 30))))
    (is (= 2000 (:dealer-pay (hand/non-dealer-tsumo {:regular 3} 30))))
    (is (= 3900 (:dealer-pay (hand/non-dealer-tsumo {:regular 4} 30)))))
  (testing "final scores"
    (is (= [35 5 -15 -25] (hand/final-scores [25000 25000 25000 25000] 25000 30000 true 20 10)))
    (is (= [46 13 -17 -40] (hand/final-scores [35700 32400 22200 9700] 25000 30000 true 20 10)))
    (is (= [56 18 -22 -50] (hand/final-scores [35700 32400 22200 9700] 25000 30000 true 30 15)))
    (is (= [58 15 -23 -49] (hand/final-scores [38000 30000 21500 10500] 25000 30000 true 30 15))))
  (testing "Mahjong Soul ranking"
    (is (= [27 9 -12 -23] (hand/final-scores [36300 28800 17900 17000] 25000 30000 false 15 5)))
    (is (= [37 14 -12 -23] (hand/mahjsoul-rank [27 9 -12 -23] :bronze :east)))))

(deftest test-shanten
  (testing "Shanten deficency number"
    (let [h (hand/grouped-hand :an [(group/straight (tile/pin 1))
                                    (group/couple (tile/pin 9))
                                    (group/tris (tile/pin 7))
                                    (group/tris (tile/pin 4))
                                    (group/tris (tile/pin 5))])]
      (is (= -1 (hand/shanten h))))

    (let [h (hand/grouped-hand :an [(tile/pin 1) (tile/pin 2)
                                    (group/couple (tile/pin 9))
                                    (group/tris (tile/pin 7))
                                    (group/tris (tile/pin 4))
                                    (group/tris (tile/pin 5))])]
      (is (= 0 (hand/shanten h))))

    (let [h (hand/grouped-hand :an [(group/straight (tile/pin 4))
                                    (group/tris (tile/man 1))
                                    (group/couple (tile/wind :east))
                                    (group/couple (tile/dragon :white))
                                    (tile/sou 2) (tile/sou 4) (tile/sou 6)])]
      (is (= 1 (hand/shanten h))))

    (let [h (hand/grouped-hand :an [(group/straight (tile/pin 4))
                                    (group/tris (tile/man 1))
                                    (tile/wind :east) (tile/wind :south)
                                    (group/couple (tile/dragon :white))
                                    (tile/sou 2) (tile/sou 4) (tile/sou 6)])]
      (is (= 2 (hand/shanten h))))

    (let [h (hand/grouped-hand :an [(tile/pin 4) (tile/pin 5) (tile/pin 7)
                                    (group/tris (tile/man 1))
                                    (tile/wind :east) (tile/wind :south)
                                    (group/couple (tile/dragon :white))
                                    (tile/sou 2) (tile/sou 4) (tile/sou 6)])]
      (is (= 3 (hand/shanten h))))
    (let [h (hand/grouped-hand :an [(tile/man 8) (tile/man 9) (tile/sou 2)
                                    (tile/sou 3) (tile/sou 4) (tile/sou 5)
                                    (tile/sou 6) (tile/sou 7) (tile/pin 1)
                                    (tile/pin 2) (tile/pin 3) (tile/pin 4)
                                    (tile/pin 5)])]
      (is (= 1 (hand/shanten h))))))

(deftest test-ukeire
  (testing "ukeire candidates"
    (let [h (hand/grouped-hand :an (tile/tiles :man [1 1 1 2 3 4 5 6 7 8 9 9 9]))
          step-by-step (-> (hand/split-tiles-groups (:an h))
                           (hand/split-ukeire-candidates)
                           (hand/split-ukeire-candidates)
                           (hand/split-ukeire-candidates)
                           (hand/split-ukeire-candidates)
                           (:tiles)
                           (set))
          candidates (hand/ukeire-candidate-tiles h)]
      (is (= (set (tile/tiles :man (range 1 10))) candidates))
      (is (= step-by-step candidates)))

    (let [h (hand/grouped-hand :an (tile/tiles :man [1 1 2 2 3 3 4 4 5 5 6 6 9]))
          candidates (hand/ukeire-candidate-tiles h)]
      (is (= #{(tile/man 9)} candidates))))
  (testing "ukeire"
    (let [h (hand/grouped-hand :an [(group/straight (tile/man 1))
                                    (group/straight (tile/man 2))
                                    (group/straight (tile/man 5))
                                    (group/couple (tile/man 7))
                                    (tile/pin 6) (tile/pin 6)])
          e #{(tile/man 1) (tile/man 4) (tile/man 7) (tile/pin 6)}
          r (hand/ukeire h)]
      (is (= e r)))
    (is (= #{(tile/sou 2)}
           (hand/ukeire (hand/grouped-hand :an [(group/straight (tile/man 1))
                                                (group/straight (tile/man 4))
                                                (group/straight (tile/man 7))
                                                (group/tris (tile/pin 1))
                                                (tile/sou 2)]))))))

(deftest test-grouping-tiles
  (testing "Recognize chiitoitsu hand"
    (let [h (hand/hand :an (apply concat (map #(tile/couple (apply tile/tile %))
                                              [[:sou 1] [:sou 5] [:pin 3]
                                               [:man 4] [:man 6]
                                               [:pin 4] [:sou 9]])))]
      (is (= 7 (count (:an (hand/grouped h))))))
    (let [gh (hand/grouped-hand :an (mapv tile/man [1 1 3 3 4 4 5 5 6 6 7 7 9 9]))]
      (is (= 7 (count (:an gh)))) "This hand is chiitoitsu"))

  (testing "Recognize regular hand"
    (let [hand (hand/hand :an (concat (tile/straight (tile/pin 7))
                                      (tile/straight (tile/pin 1))
                                      (tile/tris (tile/dragon :green))
                                      (tile/straight (tile/man 1))
                                      (tile/couple (tile/sou 1))))]
      (is (= 5 (count (:an (hand/grouped hand))))))
    (is (= (hand/hand :an (conj (mapv group/tris [(tile/man 1) (tile/man 2) (tile/man 3) (tile/man 4)])
                                (group/couple (tile/man 5))))
           (hand/grouped-hand :an (mapv tile/man [1 1 1 2 2 2 3 3 3 4 4 4 5 5]))))
    (is (= (hand/hand :an [(group/straight (tile/man 1)) (group/tris (tile/man 1))
                           (group/couple (tile/man 5)) (group/tris (tile/man 7))
                           (group/tris (tile/man 8))])
           (hand/grouped-hand :an (mapv tile/man [1 1 1 1 2 3 5 5 7 7 7 8 8 8]))))
    (let [gh (hand/grouped-hand :an (mapv tile/man [1 1 2 2 3 3 4 4 5 5 6 6 7 7]))]
      (is (= 5 (count (:an gh))) "this hand is not chiitoitsu, but ryanpeikou"))
    (let [gh (hand/grouped-hand :an (apply concat (map #(tile/couple (apply tile/tile %))
                                                       [[:sou 1] [:sou 5] [:pin 3]
                                                        [:man 4] [:man 6]
                                                        [:pin 4] [:sou 9]])))]
      (is (= 7 (count (:an gh)))))
    (let [gh (hand/grouped-hand :an (concat (tile/couple (tile/dragon :red))
                                            (mapv tile/man [2 3 4 5 5 6 7 8 9 9 9])))]
      ;;(hand/space-left gh)
      (is (= 5 (count (:an gh))))
      (is (every? :kind (:an gh)))
      (is (= 0 (hand/shanten gh))))
    (let [gh (hand/grouped-hand :an (mapv tile/man [1 2 3 4 5 5 5 6 7 8 9 9 9]))]
      (is (= 0 (hand/shanten gh))))
    (let [gh (hand/grouped-hand :an (concat (mapv tile/pin [5 6 7 7 8])
                                            (mapv tile/man [2 2 2 3 4 4 5 6])))]
      (is (hand/tenpai? gh)))
    (let [gh (hand/grouped-hand :min [(group/tris (tile/wind :east))]
                                :an (tile/tiles :man [2 2 2 3 4] :sou [2 2 2] :pin [5 6 7]))]
      (is (= [(group/couple (tile/man 2)) (group/straight (tile/man 2))
              (group/tris (tile/sou 2)) (group/straight (tile/pin 5))] (:an gh)))
      (is (= -1 (hand/shanten gh)))
      (is (not (hand/tenpai? gh))))
    (let [h (hand/grouped-hand :an (tile/tiles :man [4 5 6] :pin [1 1 1 2 3 3 4 5 6 7 8])
                          :agaripai (tile/pin 3))]
      (is (hand/regular? h))
      (is (= #{:penchan :ryanmen} (hand/machi h)))
      (is (hand/pinfu? h))))
  
  (testing "Open regular hand"
    (let [d1 {:visited [] :not-visited [(tile/man 2) (tile/man 2) (tile/man 2) (tile/sou 2) (tile/sou 2)]}
          d2 (hand/group-branch-n-bound d1)]
      (is (= 2 (count d2)))
      (is (= 0 (hand/lower-evaluation d2)))
      (is (= 10 (hand/objective-fn d2))))
    (let [h (hand/grouped-hand :min (group/groups
                                     :tris [(tile/man 2) (tile/redfive :man) (tile/sou 4)])
                               :an [(tile/man 2) (tile/man 2) (tile/man 2) (tile/sou 2) (tile/sou 2)])]
      (is (= -1 (hand/shanten h)))
      (is (hand/regular? h))))

  (testing "Recognize invalid hand"
    (let [g (hand/grouped-hand :an (concat (tile/straight (tile/pin 7))
                                           (tile/straight (tile/pin 1))
                                           (tile/tris (tile/dragon :green))
                                           (tile/straight (tile/man 1))
                                           [(tile/tile :sou 5) (tile/tile :sou 9)]))]
      (is (= 6 (count (:an g))))
      (is (= 4 (count (filter :kind (:an g)))))
      (is (= 2 (count (remove :kind (:an g))))))
    (let [g (hand/grouped-hand :an (tile/tiles :man [1 1 1 2 3 5 9 9]
                                               :sou [1 1 1]
                                               :pin [2 2 2]))]
      (is (= 6 (count (:an g))))
      (is (= 5 (count (filter :kind (:an g)))))
      (is (= 1 (count (remove :kind (:an g)))))))

  (testing "Group branch and bound"
    (is (= [(group/straight (tile/man 1)) (group/tris (tile/man 1))
            (group/tris (tile/man 5)) (group/tris (tile/man 7))
            (group/couple (tile/man 9))]
           (:visited (hand/group-branch-n-bound
                      {:not-visited (tile/tiles :man [1 1 1, 1 2 3, 5 5 5, 7 7 7, 9 9])
                       :visited []}))))

    (is (= [(group/tris (tile/man 1)) (group/straight (tile/man 1))
            (group/tris (tile/man 5)) (group/tris (tile/man 7))
            (group/couple (tile/man 9))]
           (:visited (hand/group-branch-n-bound
                      {:not-visited (tile/tiles :man [1 2 3, 5 5 5, 7 7 7, 9 9])
                       :visited [(group/tris (tile/man 1))]}))))

    (is (= [(group/straight (tile/man 1)) (group/straight (tile/man 2))
            (group/straight (tile/man 5)) (group/straight (tile/man 5))
            (group/couple (tile/man 8))]
           (:visited (hand/group-branch-n-bound
                      {:not-visited (tile/tiles :man [1 2 2 3 3 4, 5 5, 6 6 7 7 8 8])
                       :visited []}))))

    (is (= [(group/couple (tile/man 1)) (group/straight (tile/man 1))
            (group/tris (tile/man 9)) (group/tris (tile/sou 1))
            (group/tris (tile/pin 2))]
           (:visited (hand/group-branch-n-bound
                      {:not-visited (tile/tiles :man [1 1 1 2 3 9 9 9]
                                                :sou [1 1 1]
                                                :pin [2 2 2])
                       :visited []}))))

    (is (= [(group/couple (tile/man 1)) (group/couple (tile/man 9))
            (group/couple (tile/sou 1)) (group/couple (tile/sou 9))
            (group/couple (tile/pin 1)) (group/couple (tile/pin 9))
            (group/couple (tile/wind :east))]
           (:visited (hand/group-branch-n-bound
                      {:not-visited (tile/tiles :man [1 1 9 9] :sou [1 1 9 9]
                                                :pin [1 1 9 9] :wind [:east :east])
                       :visited []})))))
  
  (testing "Misc kokushi"
    (let [k (hand/hand :an tile/kokushi-tiles :agaripai (tile/man 1))
          kc (update k :an tile/conj-sort-tile (tile/man 1))
          kt (hand/hand :an (conj (vec (disj tile/kokushi-tiles (tile/man 9))) (tile/man 1)) :agaripai (tile/man 9))]
      (is (hand/juusan-menmachi? (hand/grouped k)))
      (is (not (hand/juusan-menmachi? (hand/grouped kt))))
      (is (= 1 (hand/space-left k)))
      (is (= 0 (hand/space-left kc)))
      (is (= :kokushi (hand/shape k)))
      (is (= tile/kokushi-tiles (hand/ukeire k)))
      (is (= 1 (count (hand/ukeire kt))))
      (is (= 0 (hand/shanten (hand/grouped k))))
      (is (= -1 (hand/shanten (hand/grouped kc))))
      (is (hand/tenpai? (hand/grouped k)))
      (is (not (hand/tenpai? (hand/grouped kc)))))))

(comment
  ;; TODO: REPL to proper tests
  (let [h (hand/hand :an (conj (mapv tile/man [2 3 4 5 5]) (group/quad (tile/man 1)))
                     :min (mapv group/tris [(tile/dragon :red) (tile/pin 6)]))]
    (hand/to-string (hand/grouped h)))

  (let [h (hand/hand :an (mapv tile/man [1 1 2 2 4 4 5 5 7 7 8 8 9 9]))]
    (hand/to-string (hand/grouped h)))

  (def chii (hand/hand :an (tile/tiles :man [1 1 9 9] :sou [1 1 9 9]
                                       :pin [1 1 9 9] :wind [:east :east])))
  (time (hand/shanten (hand/grouped chii))) 
  (hand/tenpai? (hand/grouped chii))
  
  (def h (hand/hand :an (tile/tiles :man (range 1 10) :pin [1 1 1 5])))
  (time (hand/ukeire h))

  (for [[han fu] [[1 30] [2 30] [3 30] [4 30] [4 50]]]
    [(hand/basic-points {:regular han} fu)
     (hand/dealer-tsumo {:regular han} fu)
     (hand/non-dealer-tsumo {:regular han} fu)
     ;;(tsumo-score han fu)
     (hand/dealer-ron {:regular han} fu)
     (hand/non-dealer-ron {:regular han} fu)
     ;;(ron-score han fu)
     (hand/string-of-score (hand/score (hand/hand) {:regular han} fu))
     ])
  (def tg {:groups [(group/tris (tile/pin 1))]
           :tiles (tile/tiles :man [1 1 2 2 3 3])})
  (-> (hand/->decomposition tg)
      (hand/group-greedy)
      (hand/split-tiles-groups)
      (hand/regular-shanten)))
