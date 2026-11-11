(ns riichi-calc.yolo-test
  (:require [clojure.test :refer [deftest testing is are]]
            [riichi-calc.vision.yolo :as yolo]))

(deftest letterbox
  (testing "wide image is padded vertically"
    (is (= {:scale 0.5 :draw-w 640.0 :draw-h 360.0 :pad-x 0.0 :pad-y 140.0}
           (yolo/letterbox 1280 720 640))))
  (testing "square image needs no padding"
    (is (= {:scale 1.0 :draw-w 640.0 :draw-h 640.0 :pad-x 0.0 :pad-y 0.0}
           (yolo/letterbox 640 640 640)))))

(deftest class-labels
  (is (= 42 (count yolo/class-labels)))
  (is (= "1B" (yolo/class-index->label 0)))
  (is (= "1D" (yolo/class-index->label 2)))
  (is (= "EW" (yolo/class-index->label 35)))
  (is (= "WW" (yolo/class-index->label 41)))
  (is (nil? (yolo/class-index->label 42))))

(deftest decode-standard
  (testing "decodes cxcywh + class scores layout [1, 4+nc, anchors]"
    (let [data [100 50        ;; cx
                100 60        ;; cy
                20 10         ;; w
                40 10         ;; h
                0.1 0.8       ;; class 0
                0.9 0.1       ;; class 1
                0.2 0.05]     ;; class 2
          boxes (yolo/decode-output data [1 7 2] 0.25)]
      (is (= 2 (count boxes)))
      (is (= {:x1 90 :y1 80 :x2 110 :y2 120 :score 0.9 :class-idx 1}
             (first boxes)))
      (is (= {:x1 45 :y1 55 :x2 55 :y2 65 :score 0.8 :class-idx 0}
             (second boxes)))))
  (testing "filters below confidence"
    (let [data [0 0 0 0 0.1 0.2 0.05]]
      (is (empty? (yolo/decode-output data [1 7 1] 0.5))))))

(deftest decode-nms-layout
  (testing "decodes [1, n, 6] with NMS already applied"
    (let [data [10 20 30 40 0.9 3
                50 60 70 80 0.3 5]
          boxes (yolo/decode-output data [1 2 6] 0.5)]
      (is (= [{:x1 10 :y1 20 :x2 30 :y2 40 :score 0.9 :class-idx 3}] boxes)))))

(deftest nms
  (let [box (fn [x1 y1 x2 y2 score cls]
              {:x1 x1 :y1 y1 :x2 x2 :y2 y2 :score score :class-idx cls})]
    (testing "suppresses overlapping boxes of the same class, keeps the best"
      (let [result (yolo/nms [(box 0 0 10 10 0.9 1)
                              (box 1 1 11 11 0.7 1)] 0.45)]
        (is (= 1 (count result)))
        (is (= 0.9 (:score (first result))))))
    (testing "keeps overlapping boxes of different classes"
      (is (= 2 (count (yolo/nms [(box 0 0 10 10 0.9 1)
                                 (box 1 1 11 11 0.7 2)] 0.45)))))
    (testing "keeps non-overlapping boxes"
      (is (= 2 (count (yolo/nms [(box 0 0 10 10 0.9 1)
                                 (box 50 50 60 60 0.7 1)] 0.45)))))))

(deftest unletterbox
  (is (= {:x1 10.0 :y1 30.0 :x2 20.0 :y2 40.0 :score 0.5 :class-idx 0}
         (yolo/unletterbox {:x1 20 :y1 100 :x2 40 :y2 120 :score 0.5 :class-idx 0}
                           {:scale 2.0 :pad-x 0.0 :pad-y 40.0}))))

(deftest box->detection
  (testing "maps a class index to a label"
    (is (= {:class "1B" :confidence 0.9 :x 15.0 :y 15.0 :width 10 :height 10}
           (yolo/box->detection {:x1 10 :y1 10 :x2 20 :y2 20
                                 :score 0.9 :class-idx 0}))))
  (testing "out of range class index yields nil"
    (is (nil? (yolo/box->detection {:x1 0 :y1 0 :x2 1 :y2 1
                                    :score 0.9 :class-idx 99})))))

(deftest postprocess
  (testing "decode + nms + unletterbox end to end"
    (let [data [100 50 100 60 20 10 40 10 0.1 0.8 0.9 0.1 0.2 0.05]
          detections (yolo/postprocess data [1 7 2]
                                       {:confidence 0.25 :iou 0.45}
                                       {:scale 1.0 :pad-x 0.0 :pad-y 0.0})]
      (is (= [{:class "1C" :confidence 0.9 :x 100.0 :y 100.0
               :width 20 :height 40}
              {:class "1B" :confidence 0.8 :x 50.0 :y 60.0
               :width 10 :height 10}]
             detections)))))
