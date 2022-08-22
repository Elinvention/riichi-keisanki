(ns riichi-calc.tile-test
  #_{:clj-kondo/ignore [:refer-all]}
  (:require [clojure.test :refer [deftest testing is are]]
            [riichi-calc.tile :refer :all]))

(deftest basic-tile
  (testing "Test basic tile functions"
    (is (= :pin (:seed (tile :pin 1))))
    (is (= 1 (:value (tile :pin 1))))
    (is (false? (:red (tile :pin 1))))
    (is (true? (:red (redfive :pin))))
    (is (= 5 (:value (redfive :pin))))
    (is (= :pin (:seed (redfive :pin))))
    (is (nil? (tile :pin :green)))
    (is (nil? (pin :green)))
    (is (nil? (redfive :green)))
    (is (numeral? (pin 1)))
    (is (not (numeral? (dragon :green))))
    (is (not (simple? (pin 1))))
    (is (simple? (pin 2)))
    (is (not (simple? (dragon :green))))
    (is (terminal? (man 1)))
    (is (not (terminal? (man 2))))
    (is (honor? (wind :east)))
    (is (honor? (dragon :white)))
    (is (not (honor? (pin 1))))
    (is (not (honor? (man 9))))
    (is (value? (wind :east) :east :east))
    (is (not (value? (pin 1) :east :east)))
    (is (value? (dragon :green) :east :east))
    (is (not (value? (wind :west) :east :east)))
    (is (value? (wind :west) :east :west))
    (is (redfive? (redfive :pin)))
    (is (not (redfive? (pin 5))))))

(deftest tiles-predicates
  (testing "Testing predicates for vector of tiles"
    (is (couple? [(man 2) (man 2)]))
    (is (not (couple? [(man 2) (man 3)])))
    (is (not (couple? [(sou 2) (man 2)])))
    (is (tris? (mapv pin [1 1 1])))
    (is (not (tris? (mapv pin [1 2 3]))))
    (is (not (quad? (mapv pin [1 1 1]))))
    (is (quad? (mapv pin [1 1 1 1])))
    (is (not (quad? (mapv pin [1 2 3 4]))))
    (is (straight? (mapv pin [1 2 3])))
    (is (not (straight? (mapv pin [1 3 4]))))
    (is (not (straight? (mapv pin [1 1 1]))))
    (is (not (straight? (mapv pin [9 1 2]))))
    (is (straight? (mapv pin [1 3 2])))
    (is (not (straight? (tris (dragon :green)))))
    (is (edge? (mapv pin [1 2 3])))
    (is (not (edge? (mapv pin [1 2 4]))))
    (is (not (edge? (mapv pin [1 1 3]))))
    (is (not (edge? (mapv pin [1 1 1]))))
    (is (edge? (mapv pin [7 8 9])))
    (is (not (edge? (mapv pin [9 1 2]))))))

(deftest make-tiles
  (testing "Make vector of tiles"
    (is (= 3 (count (straight (pin 1)))))
    (is (= 3 (count (straight (pin 7)))))
    (is (nil? (straight (sou 9))))
    (is (nil? (straight (man 0))))
    (is (nil? (straight (wind :red))))
    (is (vector? (straight (pin 5))))

    (is (= 4 (count (quad (man 9)))))
    (is (nil? (quad (wind 1))))
    (is (vector? (quad (pin 5))))
    (is (nil? (quad (pin 0))))

    (is (= 3 (count (tris (sou 1)))))
    (is (nil? (tris (dragon 1))))
    (is (vector? (tris (pin 5))))
    (is (nil? (tris (pin 0))))

    (is (= 2 (count (couple (pin 5)))))
    (is (nil? (couple (dragon :easte))))
    (is (vector? (couple (pin 5))))
    (is (nil? (couple (pin 0))))))

(deftest test-tiles-sorting
  (testing "tile-key"
  ;;TODO: better test
    (for [seed [man sou pin] i (range 1 10) j (range 1 10)]
      (cond (= i j) (is (= (tile-key (seed i)) (tile-key (seed j))))
            (< i j) (is (< (tile-key (seed i)) (tile-key (seed j))))
            (> i j) (is (> (tile-key (seed i)) (tile-key (seed j)))))))
  (testing "sorting"
    (let [tiles (vec all-34-tiles-with-redfives)
          shuffled (shuffle tiles)
          sorted (sort-tiles shuffled)]
      (is (= sorted tiles)))))

(deftest next-tile
  (testing "Next tile"
    (is (= (pin 2)         (tile-next (pin 1))))
    (is (= (dragon :green) (tile-next (dragon :white))))
    (is (= (dragon :red)   (tile-next (dragon :green))))
    (is (= (dragon :white) (tile-next (dragon :red))))
    (is (= (wind :south)   (tile-next (wind :east))))
    (is (= (wind :west)    (tile-next (wind :south))))
    (is (= (wind :north)   (tile-next (wind :west))))
    (is (= (wind :east)    (tile-next (wind :north))))))

(deftest test-min-distance
  (testing "min-distance"
    (are [dist hand tile] (= dist (min-distance hand tile))
      4 [(pin 1) (pin 9)] (pin 5)
      0 [(pin 1) (pin 5) (pin 9)] (pin 5)
      0 [(pin 1) (pin 5) (pin 5) (dragon :green)] (pin 5)
      10 [(sou 1) (dragon :green)] (pin 5)
      10 [(dragon :white) (dragon :green)] (dragon :red)
      0 [(dragon :white) (dragon :green)] (dragon :green))))
