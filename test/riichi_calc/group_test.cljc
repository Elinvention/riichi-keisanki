(ns riichi-calc.group-test
  #_{:clj-kondo/ignore [:refer-all]}
  (:require [clojure.test :refer [deftest is are testing]]
            [riichi-calc.group :refer :all]
            [riichi-calc.tile :as tile]))

(deftest basic-grouping
  (testing "grouping"
    (are [group-kinds tiles]
         (= group-kinds (:kind (group tiles)))
      :couple (tile/couple (tile/pin 1))
      :tris (tile/tris (tile/pin 1))
      :straight (tile/straight (tile/pin 1))
      :quad (tile/quad (tile/pin 4)))
    (is (nil? (group (mapv tile/pin [1 2 3 4]))) "Too many tiles should return nil")
    (is (true? (:red (group (tile/tris (tile/redfive :pin))))))
    (is (true? (:red (group (tile/straight (tile/redfive :sou))))))
    (is (nil? (couple nil)))
    (is (nil? (couple {:seed :wind :value :green})))
    (is (nil? (couple {:seed :man :value :green})))))

(deftest group-predicates
  (testing "simple?"
    (is (not (simple? (straight (tile/man 1)))))
    (is (simple? (straight (tile/man 2))))
    (is (not (simple? (straight (tile/man 7)))))
    (is (not (simple? (quad (tile/sou 1)))))
    (is (not (simple? (quad (tile/sou 9)))))
    (is (simple? (quad (tile/sou 5))))
    (is (not (simple? (tris (tile/dragon :green)))))
    (is (not (simple? (tris (tile/wind :east))))))
  (testing "redfive?"
    (is (redfive? (group [(tile/redfive :pin) (tile/pin 5) (tile/pin 5)])))
    (is (not (redfive? (tris (tile/pin 5)))))
    (is (redfive? (red-straight (tile/man 3))))
    (is (redfive? (red-straight (tile/man 5))))
    (is (not (redfive? (red-straight (tile/man 6))))))
  (testing "count tiles"
    (is (= 3 (count-tile (tris (tile/pin 2)) (tile/pin 2))))
    (is (= 1 (count-tile (straight (tile/pin 2)) (tile/pin 2)))))
  (testing "count doras"
    (is (= 3 (count-doras (tris (tile/pin 2)) [(tile/pin 1)])))
    (is (= 4 (count-doras (quad (tile/pin 2)) [(tile/pin 1)])))
    (is (= 1 (count-doras (straight (tile/pin 1)) [(tile/pin 1)])))
    (is (= 1 (count-doras (straight (tile/pin 2)) [(tile/pin 1)])))
    (is (= 0 (count-doras (straight (tile/pin 3)) [(tile/pin 1)])))))
