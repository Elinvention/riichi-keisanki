(ns riichi-calc.tile-test
  (:require [clojure.test :refer [deftest testing is are]]
            [riichi-calc.tile :as t]))

(deftest basic-tile
  (testing "Test basic tile functions"
    (is (= :pin (:seed (t/tile :pin 1))))
    (is (= 1 (:value (t/tile :pin 1))))
    (is (false? (:red (t/tile :pin 1))))
    (is (true? (:red (t/redfive :pin))))
    (is (= 5 (:value (t/redfive :pin))))
    (is (= :pin (:seed (t/redfive :pin))))
    (is (nil? (t/tile :pin :green)))
    (is (nil? (t/pin :green)))
    (is (nil? (t/redfive :green)))
    (is (t/numeral? (t/pin 1)))
    (is (not (t/numeral? (t/dragon :green))))
    (is (not (t/simple? (t/pin 1))))
    (is (t/simple? (t/pin 2)))
    (is (not (t/simple? (t/dragon :green))))
    (is (t/terminal? (t/man 1)))
    (is (not (t/terminal? (t/man 2))))
    (is (t/honor? (t/wind :east)))
    (is (t/honor? (t/dragon :white)))
    (is (not (t/honor? (t/pin 1))))
    (is (not (t/honor? (t/man 9))))
    (is (t/value? (t/wind :east) :east :east))
    (is (not (t/value? (t/pin 1) :east :east)))
    (is (t/value? (t/dragon :green) :east :east))
    (is (not (t/value? (t/wind :west) :east :east)))
    (is (t/value? (t/wind :west) :east :west))
    (is (t/redfive? (t/redfive :pin)))
    (is (not (t/redfive? (t/pin 5))))))

(deftest tiles-predicates
  (testing "Testing predicates for vector of tiles"
    (is (t/couple? [(t/man 2) (t/man 2)]))
    (is (not (t/couple? [(t/man 2) (t/man 3)])))
    (is (not (t/couple? [(t/sou 2) (t/man 2)])))
    (is (t/tris? (mapv t/pin [1 1 1])))
    (is (not (t/tris? (mapv t/pin [1 2 3]))))
    (is (not (t/quad? (mapv t/pin [1 1 1]))))
    (is (t/quad? (mapv t/pin [1 1 1 1])))
    (is (not (t/quad? (mapv t/pin [1 2 3 4]))))
    (is (t/straight? (mapv t/pin [1 2 3])))
    (is (not (t/straight? (mapv t/pin [1 3 4]))))
    (is (not (t/straight? (mapv t/pin [1 1 1]))))
    (is (not (t/straight? (mapv t/pin [9 1 2]))))
    (is (t/straight? (mapv t/pin [1 3 2])))
    (is (not (t/straight? (t/tris (t/dragon :green)))))
    (is (t/edge? (mapv t/pin [1 2 3])))
    (is (not (t/edge? (mapv t/pin [1 2 4]))))
    (is (not (t/edge? (mapv t/pin [1 1 3]))))
    (is (not (t/edge? (mapv t/pin [1 1 1]))))
    (is (t/edge? (mapv t/pin [7 8 9])))
    (is (not (t/edge? (mapv t/pin [9 1 2]))))))

(deftest make-tiles
  (testing "Make vector of tiles"
    (is (= 3 (count (t/straight (t/pin 1)))))
    (is (= 3 (count (t/straight (t/pin 7)))))
    (is (nil? (t/straight (t/sou 9))))
    (is (some? (t/straight (t/man 0))))
    (is (nil? (t/straight (t/man 10))))
    (is (nil? (t/straight (t/wind :red))))
    (is (vector? (t/straight (t/pin 5))))

    (is (= 4 (count (t/quad (t/man 9)))))
    (is (nil? (t/quad (t/wind 1))))
    (is (vector? (t/quad (t/pin 5))))
    (is (some? (t/quad (t/pin 0))))
    (is (nil? (t/quad (t/pin 11))))

    (is (= 3 (count (t/tris (t/sou 1)))))
    (is (nil? (t/tris (t/dragon 1))))
    (is (vector? (t/tris (t/pin 5))))
    (is (some? (t/tris (t/pin 0))))
    (is (nil? (t/tris (t/pin 10))))

    (is (= 2 (count (t/couple (t/pin 5)))))
    (is (nil? (t/couple (t/dragon :easte))))
    (is (vector? (t/couple (t/pin 5))))
    (is (some? (t/couple (t/pin 0))))
    (is (nil? (t/couple (t/pin 10))))))

(deftest test-tiles-sorting
  (testing "tile-key"
  ;;TODO: better test
    (doseq [seed [t/man t/sou t/pin] i (range 1 10) j (range 1 10)]
      (cond (= i j) (is (= (t/tile-key (seed i)) (t/tile-key (seed j))))
            (< i j) (is (< (t/tile-key (seed i)) (t/tile-key (seed j))))
            (> i j) (is (> (t/tile-key (seed i)) (t/tile-key (seed j)))))))
  (testing "sorting"
    (let [tiles (vec t/all-34-tiles-with-redfives)
          shuffled (shuffle tiles)
          sorted (t/sort-tiles shuffled)]
      (is (= sorted tiles)))))

(deftest next-tile
  (testing "Next tile"
    (is (= (t/pin 2)         (t/tile-next (t/pin 1))))
    (is (= (t/dragon :green) (t/tile-next (t/dragon :white))))
    (is (= (t/dragon :red)   (t/tile-next (t/dragon :green))))
    (is (= (t/dragon :white) (t/tile-next (t/dragon :red))))
    (is (= (t/wind :south)   (t/tile-next (t/wind :east))))
    (is (= (t/wind :west)    (t/tile-next (t/wind :south))))
    (is (= (t/wind :north)   (t/tile-next (t/wind :west))))
    (is (= (t/wind :east)    (t/tile-next (t/wind :north))))))

(deftest test-min-distance
  (testing "min-distance"
    (are [dist hand tile] (= dist (t/min-distance hand tile))
      4 [(t/pin 1) (t/pin 9)] (t/pin 5)
      0 [(t/pin 1) (t/pin 5) (t/pin 9)] (t/pin 5)
      0 [(t/pin 1) (t/pin 5) (t/pin 5) (t/dragon :green)] (t/pin 5)
      10 [(t/sou 1) (t/dragon :green)] (t/pin 5)
      10 [(t/dragon :white) (t/dragon :green)] (t/dragon :red)
      0 [(t/dragon :white) (t/dragon :green)] (t/dragon :green))))

(deftest notation
  (testing "to-notation"
    (are [notation tile] (= notation (t/to-notation tile))
      "1" (t/tile :sou 1)
      "9" (t/tile :sou 9)
      "0" (t/tile :sou 5 true)
      "1" (t/tile :wind :east)
      "2" (t/tile :wind :south)
      "3" (t/tile :wind :west)
      "4" (t/tile :wind :north)
      "5" (t/tile :dragon :white)
      "6" (t/tile :dragon :green)
      "7" (t/tile :dragon :red)))
  
  (testing "from-notation"
    (are [tile notation] (= tile (t/from-notation notation))
      (t/sou 1) "1s"
      (t/wind :east) "1z"
      (t/dragon :white) "5z"
      (t/dragon :red) "7z"
      nil "8z"
      nil "1")))
      