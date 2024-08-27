(ns riichi-calc.hand-test
  (:require [clojure.test :refer [deftest is are testing]]
            [riichi-calc.hand :as h]
            [riichi-calc.group :as g]
            [riichi-calc.tile :as t]))


(def kokushi-hand (h/hand :an t/kokushi-tiles :agaripai (t/dragon :red)))
(def complete-kokushi (update kokushi-hand :an conj (t/dragon :red)))
(def grouped-kokushi (h/grouped complete-kokushi))

(deftest test-machi
  (testing "machi"
    (is (= #{:penchan} (h/machi (h/hand :an [(g/straight (t/man 1))] :agaripai (t/man 3)))))
    (is (= #{:ryanmen} (h/machi (h/hand :an [(g/straight (t/man 3))] :agaripai (t/man 3)))))
    (is (= #{:kanchan} (h/machi (h/hand :an [(g/straight (t/man 3))] :agaripai (t/man 4)))))
    (is (= #{:shanpon} (h/machi (h/hand :an [(g/tris (t/man 3))] :agaripai (t/man 3)))))
    (is (= #{:tanki} (h/machi (h/hand :an [(g/couple (t/man 3))] :agaripai (t/man 3)))))
    (is (= #{:penchan :ryanmen} (h/machi (h/hand :an [(g/straight (t/pin 1))
                                                            (g/straight (t/pin 3))]
                                                       :agaripai (t/pin 3)))))))

(deftest yaku-predicates
  (testing "valid?"
    (is (not (h/valid? (h/hand))))
    (is (not (h/valid? (h/hand :an (repeat 5 (g/quad (t/pin 1)))))))
    (is (not (h/valid? (h/hand :an (vec (repeat 5 (g/quad (t/pin 1))))))))
    (is (h/valid? (h/hand :an (conj (mapv #(g/tris (t/pin %)) [1 2 3 4]) (g/couple (t/pin 9))))))
    (is (h/valid? (h/hand :an (repeat 7 (g/couple (t/pin 1))))))
    (is (h/valid? (h/hand :an (mapv #(g/couple (t/pin %)) (range 1 8)))))
    (is (h/valid? grouped-kokushi)))
  (testing "yakuhai-han"
    (let [h (h/hand :an (g/groups :tris [(t/wind :east) (t/man 2)]
                                    :straight [(t/pin 3) (t/pin 7)]
                                    :couple [(t/sou 5)]))]
      (are [han hand] (= han (h/count-yakuhai hand))
        2 h
        1 (assoc h :jikaze :west)
        1 (assoc h :jikaze :south)
        1 (assoc h :jikaze :north)
        1 (assoc h :bakaze :west)
        1 (assoc h :bakaze :south)
        1 (assoc h :bakaze :north)
        0 (assoc h :jikaze :west :bakaze :west))))
  (testing "sanshoku-doujin"
    (is (h/sanshoku-doujun? (h/hand :an [(g/straight (t/pin 1)) (g/straight (t/sou 1))
                                               (g/straight (t/man 1))])))
    (is (h/sanshoku-doujun? (h/hand :an [(g/straight (t/pin 1)) (g/straight (t/pin 1))
                                               (g/straight (t/man 1)) (g/straight (t/sou 1))])))
    (is (not (h/sanshoku-doujun? (h/hand :an [(g/straight (t/pin 1)) (g/straight (t/sou 2))
                                                    (g/straight (t/man 1)) (g/straight (t/man 2))])))))
  (testing "full-flush"
    (is (h/chinitsu? (h/hand :an [(g/straight (t/pin 1)) (g/straight (t/pin 2))
                                        (g/straight (t/pin 3)) (g/tris (t/pin 9))
                                        (g/couple (t/pin 5))])))
    (is (not (h/chinitsu? (h/hand :an [(g/straight (t/sou 1)) (g/straight (t/pin 1))
                                             (g/straight (t/pin 1)) (g/tris (t/pin 9))
                                             (g/couple (t/pin 5))]))))
    (is (not (h/chinitsu? (h/hand :an [(g/tris (t/dragon :green)) (g/quad (t/dragon :red))
                                             (g/straight (t/pin 1)) (g/tris (t/pin 9))
                                             (g/couple (t/pin 5))])))))
  (testing "half-flush"
    (is (not (h/honitsu? (h/hand :an [(g/tris (t/pin 4)) (g/quad (t/pin 6))
                                            (g/straight (t/pin 1)) (g/tris (t/pin 9))
                                            (g/couple (t/pin 5))]))))
    (is (h/honitsu? (h/hand :an [(g/tris (t/dragon :green)) (g/quad (t/dragon :red))
                                       (g/straight (t/pin 1)) (g/tris (t/pin 9))
                                       (g/couple (t/pin 5))]))))
  (testing "iipeikou"
    (is (h/iipeikou? (h/hand :an [(g/straight (t/pin 1)) (g/straight (t/pin 1))
                                        (g/straight (t/pin 7)) (g/quad (t/pin 2))
                                        (g/couple (t/man 1))])))
    (is (not (h/iipeikou? (h/hand :an [(g/straight (t/pin 1)) (g/straight (t/pin 1))
                                             (g/straight (t/pin 7)) (g/straight (t/pin 7))
                                             (g/couple (t/pin 5))]))))
    (is (h/iipeikou? (h/hand :an [(g/straight (t/pin 1)) (g/straight (t/pin 1))
                                        (g/straight (t/sou 5)) (g/straight (t/pin 7))
                                        (g/couple (t/pin 5))]))))
  (testing "chuuren poutou"
    (is (h/chuuren-poutou? (h/hand :an [(g/tris (t/man 1)) (g/straight (t/man 1))
                                              (g/straight (t/man 4)) (g/straight (t/man 7))
                                              (g/couple (t/man 9))]))))
  (testing "dora?"
    (is (not (h/dora? (h/hand) (t/man 2))))
    (is (h/dora? (h/hand :dorahyouji [(t/man 1)]) (t/man 2)))))

(deftest counting
  (testing "count-doras"
    (let [h (h/hand :an [(g/straight (t/pin 1)) (g/straight (t/pin 1))
                            (g/straight (t/pin 7)) (g/tris (t/pin 4))
                            (g/couple (t/pin 5))]
                       :dorahyouji [(t/pin 9) (t/pin 6)])]
      (is (= 3 (h/count-doras h)))))
  (testing "count redfives"
    (let [h (h/grouped-hand :an [(g/red-straight (t/pin 3))] :agaripai (t/pin 3))]
      (is (= 1 (h/count-redfive h)))))
  (testing "space left"
    (is (= 14 (h/space-left (h/hand))) "Empty hand => 14 tiles to go")
    (let [h (h/grouped-hand :an [(t/man 1)])]
      (is (= 1 (count (h/expand h))))
      (is (= 13 (h/space-left h))))
    (let [h (h/grouped-hand :an (mapv
                                    #(apply t/tile %)
                                    [[:man 1] [:man 1] [:man 2]
                                     [:sou 1] [:sou 2] [:sou 3]
                                     [:sou 4] [:sou 5] [:sou 6]
                                     [:sou 7] [:sou 8] [:sou 9]
                                     [:pin 1]]))]
      (is (= 13 (count (h/expand h))))
      (is (= 1 (h/space-left h))))
    (let [h (h/grouped-hand :an (mapv
                                    #(apply t/tile %)
                                    [[:sou 1] [:sou 2] [:sou 3]
                                     [:sou 4] [:sou 5] [:sou 6]
                                     [:sou 7] [:sou 8] [:sou 9]])
                               :min [(g/quad (t/pin 1))])]
      (is (= 13 (count (h/expand h))))
      (is (= 2 (h/space-left h))))))

(deftest list-yakus-test
  (testing "Yaku and Han computation"
    (let [h (h/hand :an [(g/straight (t/pin 1)) (g/straight (t/pin 1))
                            (g/straight (t/pin 7)) (g/straight (t/pin 4))
                            (g/couple (t/pin 5))]
                       :agari :tsumo, :bakaze :east, :jikaze :east
                       :agaripai (t/pin 1), :dorahyouji [(t/pin 9)])]
      (is (=
           {:dora 2, :menzen-tsumo 1, :iipeikou 1, :pinfu 1, :chinitsu 6 :ittsu 2}
           (h/list-yakus h))))

    (let [h (h/hand :an [(g/straight (t/pin 1)) (g/straight (t/pin 7))
                            (g/straight (t/pin 4)) (g/couple (t/pin 5))]
                       :min [(g/straight (t/pin 1))]
                       :agari :tsumo, :bakaze :east, :jikaze :east
                       :agaripai (t/pin 1), :dorahyouji [(t/pin 9)])]
      (is (= {:dora 2, :chinitsu 5, :ittsu 1} (h/list-yakus h))))

    (let [h (h/hand :an [(g/tris (t/wind :east)) (g/straight (t/pin 7))
                            (g/straight (t/pin 4)) (g/couple (t/pin 5))]
                       :min [(g/tris (t/dragon :white))]
                       :agari :tsumo, :bakaze :east, :jikaze :east
                       :agaripai (t/pin 4), :dorahyouji [(t/dragon :green)])]
      (is (= {:yakuhai 2, :honitsu 2} (h/list-yakus h))))

    (is (= 1
           (:yakuman
            (let [h (h/hand :an [(g/tris (t/sou 9)) (g/tris (t/pin 1))
                                    (g/tris (t/man 1)) (g/couple (t/sou 1))]
                               :min [(g/tris (t/pin 9))]
                               :agari :tsumo, :bakaze :east, :jikaze :east
                               :dorahyouji [(t/dragon :white)])]
              (h/hans (h/list-yakus h))))))

    (is (= (let [h (h/hand :an [(g/straight (t/pin 1)) (g/straight (t/pin 4))
                                   (g/straight (t/pin 7)) (g/straight (t/man 1))
                                   (g/couple (t/sou 1))]
                              :agari :ron, :bakaze :east, :jikaze :north
                              :dorahyouji [(t/dragon :red)])]
             (h/list-yakus h))
           {:ittsu 2}))
    (is (= (let [h (h/hand :an [(g/tris (t/man 1)) (g/straight (t/man 2))
                                   (g/couple (t/man 6)) (g/straight (t/man 4))
                                   (g/straight (t/sou 7))]
                              :agari :tsumo, :bakaze :east, :jikaze :north
                              :dorahyouji [(t/sou 8)])]
             (h/list-yakus h))
           {:dora 1, :menzen-tsumo 1}))))


(deftest minipoints-test
  (testing "has-value-couple?"
    (is (h/has-value-couple? (h/hand :an [(g/couple (t/dragon :red))])))
    (is (not (h/has-value-couple? (h/hand :an [(g/couple (t/man 1))])))))
  (testing "minipoints for all"
    (is (= 50 (let [h (h/hand :an [(g/tris (t/dragon :white)) (g/tris (t/wind :east))
                                   (g/straight (t/pin 7)) (g/straight (t/pin 4))
                                   (g/couple (t/pin 5))]
                              :agari :ron, :jikaze :west)]
                (h/minipoints h))))

    (is (= 25 (let [t [(t/pin 2) (t/sou 3) (t/man 4) (t/pin 5)
                       (t/sou 6) (t/pin 7) (t/pin 8)]
                    h (h/hand :an (mapv g/couple t))]
                (h/minipoints h))))
    
    (is (= 46 (let [h (h/hand :an [(g/tris (t/dragon :white)) (g/tris (t/wind :east))
                                   (g/straight (t/pin 7)) (g/straight (t/pin 4))
                                   (g/couple (t/pin 5))]
                              :agari :ron, :jikaze :west)]
                (->> (h/minipoints-step-by-step h)
                     (vals)
                     (flatten)
                     (apply +)))))))

(deftest scoring
  (testing "rounding"
    (is (= 13 (h/round-thousandth 12500)))
    (is (= 13 (h/round-thousandth 12400)))
    (is (= 13 (h/round-thousandth 12600))))
  (testing "dealer ron 1 han"
    (is (= 1500 (:ron-pay (h/dealer-ron {:regular 1} 30))))
    (is (= 2000 (:ron-pay (h/dealer-ron {:regular 1} 40))))
    (is (= 2400 (:ron-pay (h/dealer-ron {:regular 1} 50))))
    (is (= 2900 (:ron-pay (h/dealer-ron {:regular 1} 60))))
    (is (= 3400 (:ron-pay (h/dealer-ron {:regular 1} 70))))
    (is (= 3900 (:ron-pay (h/dealer-ron {:regular 1} 80))))
    (is (= 4400 (:ron-pay (h/dealer-ron {:regular 1} 90))))
    (is (= 4800 (:ron-pay (h/dealer-ron {:regular 1} 100))))
    (is (= 5300 (:ron-pay (h/dealer-ron {:regular 1} 110)))))
  (testing "dealer ron 2 han"
    (is (= 2900 (:ron-pay (h/dealer-ron {:regular 2} 30))))
    (is (= 3900 (:ron-pay (h/dealer-ron {:regular 2} 40))))
    (is (= 4800 (:ron-pay (h/dealer-ron {:regular 2} 50))))
    (is (= 5800 (:ron-pay (h/dealer-ron {:regular 2} 60))))
    (is (= 6800 (:ron-pay (h/dealer-ron {:regular 2} 70))))
    (is (= 7700 (:ron-pay (h/dealer-ron {:regular 2} 80))))
    (is (= 8700 (:ron-pay (h/dealer-ron {:regular 2} 90))))
    (is (= 9600 (:ron-pay (h/dealer-ron {:regular 2} 100))))
    (is (= 10600 (:ron-pay (h/dealer-ron {:regular 2} 110)))))
  (testing "TODO: finire test"
    (is (= 5800 (:ron-pay (h/dealer-ron {:regular 3} 30))))
    (is (= 7700 (:ron-pay (h/dealer-ron {:regular 3} 40))))
    (is (= 11600 (:ron-pay (h/dealer-ron {:regular 4} 30))))
    (is (= 12000 (:ron-pay (h/dealer-ron {:regular 4} 40)))))
  (testing "non dealer ron"
    (is (= 1000 (:ron-pay (h/non-dealer-ron {:regular 1} 30))))
    (is (= 2000 (:ron-pay (h/non-dealer-ron {:regular 2} 30))))
    (is (= 3900 (:ron-pay (h/non-dealer-ron {:regular 3} 30))))
    (is (= 7700 (:ron-pay (h/non-dealer-ron {:regular 4} 30)))))
  (testing "dealer tsumo"
    (is (= 500 (:everyone-pay (h/dealer-tsumo {:regular 1} 30))))
    (is (= 1000 (:everyone-pay (h/dealer-tsumo {:regular 2} 30))))
    (is (= 2000 (:everyone-pay (h/dealer-tsumo {:regular 3} 30))))
    (is (= 3900 (:everyone-pay (h/dealer-tsumo {:regular 4} 30)))))
  (testing "non dealer tsumo"
    (is (= 500 (:dealer-pay (h/non-dealer-tsumo {:regular 1} 30))))
    (is (= 1000 (:dealer-pay (h/non-dealer-tsumo {:regular 2} 30))))
    (is (= 2000 (:dealer-pay (h/non-dealer-tsumo {:regular 3} 30))))
    (is (= 3900 (:dealer-pay (h/non-dealer-tsumo {:regular 4} 30)))))
  (testing "kazoe-yakuman"
    (is (= 48000 (:ron-pay (h/dealer-ron {:regular 13} 30))))
    (is (= 32000 (:ron-pay (h/non-dealer-ron {:regular 13} 30))))
    (is (= 16000 (:everyone-pay (h/dealer-tsumo {:regular 13} 30))))
    (is (= {:dealer-pay 16000, :non-dealer-pay 8000}
           (h/non-dealer-tsumo {:regular 13} 30))))
  (testing "final scores"
    (is (= [35 5 -15 -25] (h/final-scores [25000 25000 25000 25000] 25000 30000 true 20 10)))
    (is (= [46 13 -17 -40] (h/final-scores [35700 32400 22200 9700] 25000 30000 true 20 10)))
    (is (= [56 18 -22 -50] (h/final-scores [35700 32400 22200 9700] 25000 30000 true 30 15)))
    (is (= [58 15 -23 -49] (h/final-scores [38000 30000 21500 10500] 25000 30000 true 30 15))))
  (testing "Mahjong Soul ranking"
    (is (= [27 9 -12 -23] (h/final-scores [36300 28800 17900 17000] 25000 30000 false 15 5)))
    (is (= [37 14 -12 -23] (h/mahjsoul-rank [27 9 -12 -23] :bronze :east)))))

(deftest test-shanten
  (testing "Shanten deficency number"
    (let [h (h/grouped-hand :an [(g/straight (t/pin 1))
                                    (g/couple (t/pin 9))
                                    (g/tris (t/pin 7))
                                    (g/tris (t/pin 4))
                                    (g/tris (t/pin 5))])]
      (is (= -1 (h/shanten h))))

    (let [h (h/grouped-hand :an [(t/pin 1) (t/pin 2)
                                    (g/couple (t/pin 9))
                                    (g/tris (t/pin 7))
                                    (g/tris (t/pin 4))
                                    (g/tris (t/pin 5))])]
      (is (= 0 (h/shanten h))))

    (let [h (h/grouped-hand :an [(g/straight (t/pin 4))
                                    (g/tris (t/man 1))
                                    (g/couple (t/wind :east))
                                    (g/couple (t/dragon :white))
                                    (t/sou 2) (t/sou 4) (t/sou 6)])]
      (is (= 1 (h/shanten h))))

    (let [h (h/grouped-hand :an [(g/straight (t/pin 4))
                                    (g/tris (t/man 1))
                                    (t/wind :east) (t/wind :south)
                                    (g/couple (t/dragon :white))
                                    (t/sou 2) (t/sou 4) (t/sou 6)])]
      (is (= 2 (h/shanten h))))

    (let [h (h/grouped-hand :an [(t/pin 4) (t/pin 5) (t/pin 7)
                                    (g/tris (t/man 1))
                                    (t/wind :east) (t/wind :south)
                                    (g/couple (t/dragon :white))
                                    (t/sou 2) (t/sou 4) (t/sou 6)])]
      (is (= 3 (h/shanten h))))
    (let [h (h/grouped-hand :an [(t/man 8) (t/man 9) (t/sou 2)
                                    (t/sou 3) (t/sou 4) (t/sou 5)
                                    (t/sou 6) (t/sou 7) (t/pin 1)
                                    (t/pin 2) (t/pin 3) (t/pin 4)
                                    (t/pin 5)])]
      (is (= 1 (h/shanten h))))))

(deftest test-ukeire
  (testing "ukeire candidates"
    (let [h (h/grouped-hand :an (t/tiles :man [1 1 1 2 3 4 5 6 7 8 9 9 9]))
          step-by-step (-> (h/split-tiles-groups (:an h))
                           (h/split-ukeire-candidates)
                           (h/split-ukeire-candidates)
                           (h/split-ukeire-candidates)
                           (h/split-ukeire-candidates)
                           (:tiles)
                           (set))
          candidates (h/ukeire-candidate-tiles h)]
      (is (= (set (t/tiles :man (range 1 10))) candidates))
      (is (= step-by-step candidates)))

    (let [h (h/grouped-hand :an (t/tiles :man [1 1 2 2 3 3 4 4 5 5 6 6 9]))
          candidates (h/ukeire-candidate-tiles h)]
      (is (= #{(t/man 9)} candidates))))
  (testing "ukeire"
    (let [h (h/grouped-hand :an [(g/straight (t/man 1))
                                    (g/straight (t/man 2))
                                    (g/straight (t/man 5))
                                    (g/couple (t/man 7))
                                    (t/pin 6) (t/pin 6)])
          e #{(t/man 1) (t/man 4) (t/man 7) (t/pin 6)}
          r (h/ukeire h)]
      (is (= e r)))
    (is (= #{(t/sou 2)}
           (h/ukeire (h/grouped-hand :an [(g/straight (t/man 1))
                                                (g/straight (t/man 4))
                                                (g/straight (t/man 7))
                                                (g/tris (t/pin 1))
                                                (t/sou 2)]))))))

(deftest test-grouping-tiles 
  (testing "grouped-tiles"
    (are [grouped tiles] (= grouped (h/grouped-tiles tiles))
      [(g/tris (t/man 1))] (t/tiles :man [1 1 1])
      [(g/tris (t/man 2))] (t/tiles :man [2 2 2])
      (g/groups :tris (t/tiles :man [1 5 9])) (t/tiles :man [1 1 1 5 5 5 9 9 9])))
  
  (testing "Recognize chiitoitsu hand"
    (let [h (h/hand :an (apply concat (map #(t/couple (apply t/tile %))
                                              [[:sou 1] [:sou 5] [:pin 3]
                                               [:man 4] [:man 6]
                                               [:pin 4] [:sou 9]])))]
      (is (= 7 (count (:an (h/grouped h))))))
    (let [gh (h/grouped-hand :an (mapv t/man [1 1 3 3 4 4 5 5 6 6 7 7 9 9]))]
      (is (= 7 (count (:an gh)))) "This hand is chiitoitsu"))

  (testing "Recognize regular hand"
    (let [hand (h/hand :an (concat (t/straight (t/pin 7))
                                      (t/straight (t/pin 1))
                                      (t/tris (t/dragon :green))
                                      (t/straight (t/man 1))
                                      (t/couple (t/sou 1))))]
      (is (= 5 (count (:an (h/grouped hand))))))
    (is (= (h/hand :an (conj (mapv g/tris [(t/man 1) (t/man 2) (t/man 3) (t/man 4)])
                                (g/couple (t/man 5))))
           (h/grouped-hand :an (mapv t/man [1 1 1 2 2 2 3 3 3 4 4 4 5 5]))))
    (is (= (h/hand :an [(g/straight (t/man 1)) (g/tris (t/man 1))
                           (g/couple (t/man 5)) (g/tris (t/man 7))
                           (g/tris (t/man 8))])
           (h/grouped-hand :an (mapv t/man [1 1 1 1 2 3 5 5 7 7 7 8 8 8]))))
    (let [gh (h/grouped-hand :an (mapv t/man [1 1 2 2 3 3 4 4 5 5 6 6 7 7]))]
      (is (= 5 (count (:an gh))) "this hand is not chiitoitsu, but ryanpeikou"))
    (let [gh (h/grouped-hand :an (apply concat (map #(t/couple (apply t/tile %))
                                                       [[:sou 1] [:sou 5] [:pin 3]
                                                        [:man 4] [:man 6]
                                                        [:pin 4] [:sou 9]])))]
      (is (= 7 (count (:an gh)))))
    (let [gh (h/grouped-hand :an (concat (t/couple (t/dragon :red))
                                            (mapv t/man [2 3 4 5 5 6 7 8 9 9 9])))]
      ;;(hand/space-left gh)
      (is (= 5 (count (:an gh))))
      (is (every? :kind (:an gh)))
      (is (= 0 (h/shanten gh))))
    (let [gh (h/grouped-hand :an (mapv t/man [1 2 3 4 5 5 5 6 7 8 9 9 9]))]
      (is (= 0 (h/shanten gh))))
    (let [gh (h/grouped-hand :an (concat (mapv t/pin [5 6 7 7 8])
                                            (mapv t/man [2 2 2 3 4 4 5 6])))]
      (is (h/tenpai? gh)))
    (let [gh (h/grouped-hand :min [(g/tris (t/wind :east))]
                                :an (t/tiles :man [2 2 2 3 4] :sou [2 2 2] :pin [5 6 7]))]
      (is (= [(g/couple (t/man 2)) (g/straight (t/man 2))
              (g/tris (t/sou 2)) (g/straight (t/pin 5))] (:an gh)))
      (is (= -1 (h/shanten gh)))
      (is (not (h/tenpai? gh))))
    (let [h (h/grouped-hand :an (t/tiles :man [4 5 6] :pin [1 1 1 2 3 3 4 5 6 7 8])
                          :agaripai (t/pin 3))]
      (is (h/regular? h))
      (is (= #{:penchan :ryanmen} (h/machi h)))
      (is (h/pinfu? h))))
  
  (testing "Open regular hand"
    (let [d1 {:visited [] :not-visited [(t/man 2) (t/man 2) (t/man 2) (t/sou 2) (t/sou 2)]}
          d2 (h/group-branch-n-bound d1)]
      (is (= 2 (count d2)))
      (is (= 0 (h/lower-evaluation d2)))
      (is (= 10 (h/objective-fn d2))))
    (let [h (h/grouped-hand :min (g/groups 
                                     :tris [(t/redfive :man) (t/sou 4) (t/pin 2)])
                               :an [(t/man 2) (t/man 2) (t/man 2) (t/sou 2) (t/sou 2)])]
      (is (= -1 (h/shanten h)))
      (is (h/regular? h))))

  (testing "Recognize invalid hand"
    (let [g (h/grouped-hand :an (concat (t/straight (t/pin 7))
                                           (t/straight (t/pin 1))
                                           (t/tris (t/dragon :green))
                                           (t/straight (t/man 1))
                                           [(t/tile :sou 5) (t/tile :sou 9)]))]
      (is (= 6 (count (:an g))))
      (is (= 4 (count (filter :kind (:an g)))))
      (is (= 2 (count (remove :kind (:an g))))))
    (let [g (h/grouped-hand :an (t/tiles :man [1 1 1 2 3 5 9 9]
                                               :sou [1 1 1]
                                               :pin [2 2 2]))]
      (is (= 6 (count (:an g))))
      (is (= 5 (count (filter :kind (:an g)))))
      (is (= 1 (count (remove :kind (:an g)))))))

  (testing "Group branch and bound"
    (is (= [(g/straight (t/man 1)) (g/tris (t/man 1))
            (g/tris (t/man 5)) (g/tris (t/man 7))
            (g/couple (t/man 9))]
           (:visited (h/group-branch-n-bound
                      {:not-visited (t/tiles :man [1 1 1, 1 2 3, 5 5 5, 7 7 7, 9 9])
                       :visited []}))))

    (is (= [(g/tris (t/man 1)) (g/straight (t/man 1))
            (g/tris (t/man 5)) (g/tris (t/man 7))
            (g/couple (t/man 9))]
           (:visited (h/group-branch-n-bound
                      {:not-visited (t/tiles :man [1 2 3, 5 5 5, 7 7 7, 9 9])
                       :visited [(g/tris (t/man 1))]}))))

    (is (= [(g/straight (t/man 1)) (g/straight (t/man 2))
            (g/straight (t/man 5)) (g/straight (t/man 5))
            (g/couple (t/man 8))]
           (:visited (h/group-branch-n-bound
                      {:not-visited (t/tiles :man [1 2 2 3 3 4, 5 5, 6 6 7 7 8 8])
                       :visited []}))))

    (is (= [(g/couple (t/man 1)) (g/straight (t/man 1))
            (g/tris (t/man 9)) (g/tris (t/sou 1))
            (g/tris (t/pin 2))]
           (:visited (h/group-branch-n-bound
                      {:not-visited (t/tiles :man [1 1 1 2 3 9 9 9]
                                                :sou [1 1 1]
                                                :pin [2 2 2])
                       :visited []}))))

    (is (= [(g/couple (t/man 1)) (g/couple (t/man 9))
            (g/couple (t/sou 1)) (g/couple (t/sou 9))
            (g/couple (t/pin 1)) (g/couple (t/pin 9))
            (g/couple (t/wind :east))]
           (:visited (h/group-branch-n-bound
                      {:not-visited (t/tiles :man [1 1 9 9] :sou [1 1 9 9]
                                                :pin [1 1 9 9] :wind [:east :east])
                       :visited []})))))
  
  (testing "Misc kokushi"
    (let [k (h/hand :an t/kokushi-tiles :agaripai (t/man 1))
          kc (update k :an t/conj-sort-tile (t/man 1))
          kt (h/hand :an (conj (vec (disj t/kokushi-tiles (t/man 9))) (t/man 1)) :agaripai (t/man 9))]
      (is (h/juusan-menmachi? (h/grouped k)))
      (is (not (h/juusan-menmachi? (h/grouped kt))))
      (is (= 1 (h/space-left k)))
      (is (= 0 (h/space-left kc)))
      (is (= :kokushi (h/shape k)))
      (is (= t/kokushi-tiles (h/ukeire k)))
      (is (= 1 (count (h/ukeire kt))))
      (is (= 0 (h/shanten (h/grouped k))))
      (is (= -1 (h/shanten (h/grouped kc))))
      (is (h/tenpai? (h/grouped k)))
      (is (not (h/tenpai? (h/grouped kc)))))
    (is (h/juusan-menmachi? (assoc (h/kokushi-hand (t/man 1)) :agaripai (t/man 1))))
    (is (not (h/juusan-menmachi? (assoc (h/kokushi-hand (t/man 1)) :agaripai (t/sou 9)))))))

(deftest notation
  (testing "to-notation" 
    (are [notation tiles] (= notation (h/to-notation tiles))
      "12355m111s567p" (t/tiles :man [1 2 3 5 5] :sou [1 1 1] :pin [5 6 7])
      "555777z" (g/groups :tris (t/tiles :dragon [:red :white]))))
  (testing "from-notation"
    (are [tiles notation] (= tiles (h/from-notation notation))
      (t/tiles :man [1 2 3 5 5] :sou [1 1 1] :pin [5 6 7]) "12355m111s567p"
      (t/tiles :dragon [:red :red :red :white :white :white]) "555777z"
      (t/tiles :man [1 2 3 5 5] :sou [1 1 1] :pin [5 6 7] :dragon [:white :white :white :red :red :red]) "12355m111s567p555777z"
      [] "1"
      [] "11"
      [(t/sou 1)] "1s"
      (t/couple (t/sou 1)) "11s"
      (t/tris (t/sou 1)) "111s"
      (t/straight (t/sou 1)) "123s"
      (t/tiles :man [0] :sou [1 2 3] :pin [1]) "0m123s1p"
      (t/tiles :sou [1 2 3] :pin [1 2 3]) "123s123p"
      (t/tiles :sou [1 2 3] :pin [4 0 6]) "123s406p"
      (t/tiles :pin [1 1 1 1 2 3]) "111123p"
      (t/couple (t/pin 9)) "99p")))

(deftest can-add-test 
  (testing "Can add chii?"
    (is (not (h/can-add-chii? (h/hand) (t/dragon :white))))
    (is (not (h/can-add-chii? (h/hand) (t/wind :east))))
    (is (not (h/can-add-chii? (h/hand) (t/man 9))))
    (is (h/can-add-chii? (h/hand) (t/man 7)))
    (is (h/can-add-chii? (h/hand) (t/man 1)))
    (is (h/can-add-chii? (h/hand :an (t/tiles :man [1 1 1])) (t/man 1)))
    (is (not (h/can-add-chii? (h/hand :an (t/tiles :man [1 1 1 1])) (t/man 1))))))

(comment
  ;; TODO: REPL to proper tests
  (let [h (h/hand :an (conj (mapv t/man [2 3 4 5 5]) (g/quad (t/man 1)))
                     :min (mapv g/tris [(t/dragon :red) (t/pin 6)]))]
    (h/to-string (h/grouped h)))

  (let [h (h/hand :an (mapv t/man [1 1 2 2 4 4 5 5 7 7 8 8 9 9]))]
    (h/to-string (h/grouped h)))

  (def chii (h/hand :an (t/tiles :man [1 1 9 9] :sou [1 1 9 9]
                                       :pin [1 1 9 9] :wind [:east :east])))
  (time (h/shanten (h/grouped chii)))
  (h/tenpai? (h/grouped chii))

  (def h (h/hand :an (t/tiles :man (range 1 10) :pin [1 1 1 5])))
  (time (h/ukeire h))

  (for [[han fu] [[1 30] [2 30] [3 30] [4 30] [4 50]]]
    [(h/basic-points {:regular han} fu)
     (h/dealer-tsumo {:regular han} fu)
     (h/non-dealer-tsumo {:regular han} fu)
     ;;(tsumo-score han fu)
     (h/dealer-ron {:regular han} fu)
     (h/non-dealer-ron {:regular han} fu)
     ;;(ron-score han fu)
     (h/string-of-score (h/score (h/hand)))])
  (def tg {:groups [(g/tris (t/pin 1))]
           :tiles (t/tiles :man [1 1 2 2 3 3])})
  (-> (h/->decomposition tg)
      (h/group-greedy)
      (h/split-tiles-groups)
      (h/regular-shanten)) 
  )
