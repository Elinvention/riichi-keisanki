(ns riichi-calc.vision-test
  (:require [clojure.test :refer [deftest testing is are]]
            [riichi-calc.tile :as t]
            [riichi-calc.vision :as v]))

(deftest class->tile
  (testing "suits"
    (is (= (t/sou 1) (v/class->tile "1B")))
    (is (= (t/sou 9) (v/class->tile "9B")))
    (is (= (t/man 5) (v/class->tile "5C")))
    (is (= (t/pin 3) (v/class->tile "3D"))))
  (testing "honors"
    (is (= (t/wind :east) (v/class->tile "EW")))
    (is (= (t/wind :south) (v/class->tile "SW")))
    (is (= (t/wind :west) (v/class->tile "WW")))
    (is (= (t/wind :north) (v/class->tile "NW")))
    (is (= (t/dragon :green) (v/class->tile "GD")))
    (is (= (t/dragon :red) (v/class->tile "RD")))
    (is (= (t/dragon :white) (v/class->tile "WD"))))
  (testing "unsupported and invalid classes"
    (is (nil? (v/class->tile "1F")))
    (is (nil? (v/class->tile "2S")))
    (is (nil? (v/class->tile "0B")))
    (is (nil? (v/class->tile "mystery")))
    (is (nil? (v/class->tile nil)))))

(deftest parse-detections
  (testing "filters low-confidence and unsupported detections"
    (let [detections [{:class "1B" :confidence 0.9 :x 0 :y 0}
                      {:class "1C" :confidence 0.1 :x 0 :y 0}
                      {:class "1F" :confidence 0.9 :x 0 :y 0}]
          parsed (v/parse-detections detections)]
      (is (= 1 (count parsed)))
      (is (= (t/sou 1) (:tile (first parsed))))))
  (testing "orders detections left to right"
    (let [detections [{:class "1B" :confidence 0.9 :x 100 :y 0}
                      {:class "1C" :confidence 0.9 :x 0 :y 0}
                      {:class "1D" :confidence 0.9 :x 0 :y 100}]
          tiles (v/detections->tiles detections)]
      (is (= [(t/man 1) (t/pin 1) (t/sou 1)] tiles))))
  (testing "custom confidence threshold"
    (let [detections [{:class "1B" :confidence 0.3 :x 0 :y 0}]]
      (is (empty? (v/parse-detections detections 0.5)))
      (is (= [(t/sou 1)] (v/detections->tiles detections 0.2))))))
