(ns riichi-calc.game-test
  (:require [clojure.test :refer [deftest is]]
            [riichi-calc.game :as game]
            [riichi-calc.hand :as hand]))

(def hand-40-fu-1-han (hand/score {:ron-pay 1300} :south :ron 1 40))

(deftest player-wind-test
  (is (= [:east :south :west :north]
         (mapv #(game/player-wind {:dealer 0} %) (range 4))))
  (is (= [:west :north :east :south]
         (mapv #(game/player-wind {:dealer 2} %) (range 4)))))

(deftest next-dealer-test
  (is (= 0 (game/next-dealer {:dealer 0} 0)))
  (is (= 1 (game/next-dealer {:dealer 0} 1)))
  (is (= 2 (game/next-dealer {:dealer 2} 2)))
  (is (= 0 (game/next-dealer {:dealer 3} 1))))

(deftest ron-test
  (let [state {:players [{:name "Elia" :score 25000}
                         {:name "Luca" :score 25000}
                         {:name "Giangi" :score 25000}
                         {:name "Q" :score 25000}]
               :current-hand hand-40-fu-1-han}]
    (is (= [{:name "Elia" :score 26300}
            {:name "Luca" :score 23700}
            {:name "Giangi" :score 25000}
            {:name "Q" :score 25000}]
           (:players (game/ron state 0 1))))))

(def hand-30-fu-2-han-tsumo (hand/score {:dealer-pay 1000 :non-dealer-pay 500} :south :tsumo 2 30))
(deftest tsumo-non-dealer-test
  (let [state {:players [{:name "Elia" :score 25000}
                         {:name "Luca" :score 25000}
                         {:name "Giangi" :score 25000}
                         {:name "Q" :score 25000}]
               :current-hand hand-30-fu-2-han-tsumo}]
    (is (= [{:name "Elia" :score 24000}
            {:name "Luca" :score 27000}
            {:name "Giangi" :score 24500}
            {:name "Q" :score 24500}]
           (:players (game/tsumo state 1))))))

(def hand-30-fu-2-han-tsumo-dealer (hand/score {:everyone-pay 1000} :east :tsumo 2 30))
(deftest tsumo-dealer-test
  (let [state {:players [{:name "Elia" :score 25000}
                         {:name "Luca" :score 25000}
                         {:name "Giangi" :score 25000}
                         {:name "Q" :score 25000}]
               :current-hand hand-30-fu-2-han-tsumo-dealer}]
    (is (= [{:name "Elia" :score 28000}
            {:name "Luca" :score 24000}
            {:name "Giangi" :score 24000}
            {:name "Q" :score 24000}]
           (:players (game/tsumo state 0))))))
