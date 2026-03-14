(ns riichi-calc.group-test
  (:require [clojure.test :refer [deftest is are testing]]
            [riichi-calc.group :as g]
            [riichi-calc.tile :as t]))

(deftest basic-grouping
  (testing "grouping"
    (are [group-kinds tiles]
         (= group-kinds (:kind (g/group tiles)))
      :couple (t/couple (t/pin 1))
      :tris (t/tris (t/pin 1))
      :straight (t/straight (t/pin 1))
      :quad (t/quad (t/pin 4)))
    (is (nil? (g/group (mapv t/pin [1 2 3 4]))) "Too many tiles should return nil")
    (is (true? (:red (g/group (t/tris (t/redfive :pin))))))
    (is (true? (:red (g/group (t/straight (t/redfive :sou))))))
    (is (nil? (g/couple nil)))
    (is (nil? (g/couple {:seed :wind :value :green})))
    (is (nil? (g/couple {:seed :man :value :green})))))

(deftest group-predicates
  (testing "simple?"
    (is (not (g/simple? (g/straight (t/man 1)))))
    (is (g/simple? (g/straight (t/man 2))))
    (is (not (g/simple? (g/straight (t/man 7)))))
    (is (not (g/simple? (g/quad (t/sou 1)))))
    (is (not (g/simple? (g/quad (t/sou 9)))))
    (is (g/simple? (g/quad (t/sou 5))))
    (is (not (g/simple? (g/tris (t/dragon :green)))))
    (is (not (g/simple? (g/tris (t/wind :east))))))
  (testing "redfive?"
    (is (g/redfive? (g/group [(t/redfive :pin) (t/pin 5) (t/pin 5)])))
    (is (not (g/redfive? (g/tris (t/pin 5)))))
    (is (g/redfive? (g/red-straight (t/man 3))))
    (is (g/redfive? (g/red-straight (t/man 5))))
    (is (not (g/redfive? (g/red-straight (t/man 6))))))
  (testing "count tiles"
    (is (= 3 (g/count-tile (g/tris (t/pin 2)) (t/pin 2))))
    (is (= 1 (g/count-tile (g/straight (t/pin 2)) (t/pin 2)))))
  (testing "count doras"
    (is (= 3 (g/count-doras (g/tris (t/pin 2)) [(t/pin 1)])))
    (is (= 4 (g/count-doras (g/quad (t/pin 2)) [(t/pin 1)])))
    (is (= 1 (g/count-doras (g/straight (t/pin 1)) [(t/pin 1)])))
    (is (= 1 (g/count-doras (g/straight (t/pin 2)) [(t/pin 1)])))
    (is (= 0 (g/count-doras (g/straight (t/pin 3)) [(t/pin 1)])))))

(deftest notation
  (testing "to-notation"
    (are [notation group] (= notation (g/to-notation group))
      "123" (g/straight (t/pin 1))
      "111" (g/tris (t/pin 1))
      "11" (g/couple (t/pin 1))
      "1111" (g/quad (t/pin 1))
      "11" (g/couple (t/man 1))
      "11" (g/couple (t/sou 1))
      "11" (g/couple (t/pin 1))
      "11" (g/couple (t/wind :east))
      "22" (g/couple (t/wind :south))
      "33" (g/couple (t/wind :west))
      "44" (g/couple (t/wind :north))
      "55" (g/couple (t/dragon :white))
      "66" (g/couple (t/dragon :green))
      "77" (g/couple (t/dragon :red))))
  (testing "from-notation"
    (are [group notation] (= group (g/from-notation notation))
      (t/couple (t/dragon :white)) "55z"
      (t/couple (t/wind :east)) "11z"
      (t/tris (t/sou 1)) "111s"
      (t/straight (t/sou 1)) "123s"
      [(t/sou 1) (t/sou 2)] "12s"
      [(t/sou 1)] "1s"
      (t/tris (t/wind :east)) "111z"
      (t/tris (t/dragon :white)) "555z"
      (t/tris (t/dragon :green)) "666z"
      [(t/dragon :red) (t/dragon :red)] "77z"
      nil "1"
      nil "11"
      nil "111"
      (t/tiles :sou [1 1 1 1 2 3]) "111123s"
      (t/red-straight (t/pin 4)) "406p")))
