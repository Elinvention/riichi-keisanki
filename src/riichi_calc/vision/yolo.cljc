(ns riichi-calc.vision.yolo
  "Pure YOLO (Ultralytics v8/v11-style) detection post-processing: letterbox
   geometry, output decoding, IoU/NMS and coordinate mapping. No ONNX dependency,
   so it is unit-testable and platform-independent.")

(def default-input-size 640)
(def default-confidence 0.25)
(def default-iou 0.45)
(def default-model-path "assets/models/riichi.v2.yolo26s.int8.onnx")

(def class-labels
  "Default class index -> label, in the order the v1 model was trained (the
   Mahjong dataset order). Models trained with a different class list (e.g. the
   v2 models with red fives) store their own names in the ONNX metadata; see
   `labels-from-names-string`."
  ["1B" "1C" "1D" "1F" "1S"
   "2B" "2C" "2D" "2F" "2S"
   "3B" "3C" "3D" "3F" "3S"
   "4B" "4C" "4D" "4F" "4S"
   "5B" "5C" "5D"
   "6B" "6C" "6D"
   "7B" "7C" "7D"
   "8B" "8C" "8D"
   "9B" "9C" "9D"
   "EW" "GD" "NW" "RD" "SW" "WD" "WW"])

(defn labels-from-names-string
  "Parses an ONNX `names` metadata value such as
   \"{0: '1B', 1: '1C', ...}\" into a vector indexed by class id. Returns nil
   when no entries are found."
  [s]
  (when (string? s)
    (let [pairs (re-seq #"(\d+):\s*'([^']*)'" s)]
      (when (seq pairs)
        (reduce (fn [labels [_ index label]]
                  (assoc labels (parse-long index) label))
                [] pairs)))))

(defn class-index->label
  ([index] (class-index->label class-labels index))
  ([labels index] (get labels index)))

(defn letterbox
  "Geometry to fit a src-w x src-h image into a size x size canvas without
   distorting it. The image is centred and padded; pad pixels are grey (114).
   Returns {:scale :draw-w :draw-h :pad-x :pad-y}."
  [src-w src-h size]
  (let [scale (min (/ size src-w) (/ size src-h))
        draw-w (* src-w scale)
        draw-h (* src-h scale)]
    {:scale scale
     :draw-w draw-w
     :draw-h draw-h
     :pad-x (/ (- size draw-w) 2.0)
     :pad-y (/ (- size draw-h) 2.0)}))

(defn- iou [{ax1 :x1 ay1 :y1 ax2 :x2 ay2 :y2} {bx1 :x1 by1 :y1 bx2 :x2 by2 :y2}]
  (let [ix1 (max ax1 bx1)
        iy1 (max ay1 by1)
        ix2 (min ax2 bx2)
        iy2 (min ay2 by2)
        iw (- ix2 ix1)
        ih (- iy2 iy1)
        inter (if (and (pos? iw) (pos? ih)) (* iw ih) 0.0)
        area-a (* (- ax2 ax1) (- ay2 ay1))
        area-b (* (- bx2 bx1) (- by2 by1))
        union (- (+ area-a area-b) inter)]
    (if (pos? union) (/ inter union) 0.0)))

(defn nms
  "Class-aware non-maximum suppression. Boxes are maps with :x1 :y1 :x2 :y2
   :score :class-idx. Boxes of different classes never suppress each other."
  ([boxes] (nms boxes default-iou))
  ([boxes iou-threshold]
   (->> boxes
        (group-by :class-idx)
        (mapcat (fn [[_ class-boxes]]
                  (loop [remaining (sort-by :score > class-boxes)
                         kept []]
                    (if-let [best (first remaining)]
                      (recur (remove #(> (iou best %) iou-threshold)
                                     (rest remaining))
                             (conj kept best))
                      kept))))
        vec)))

(defn- decode-nms-layout
  "Decodes a model that already includes NMS: rows of [x1 y1 x2 y2 score class]."
  [data n-rows confidence]
  (into []
        (for [i (range n-rows)
              :let [base (* i 6)
                    score (nth data (+ base 4))]
              :when (>= score confidence)]
          {:x1 (nth data base)
           :y1 (nth data (+ base 1))
           :x2 (nth data (+ base 2))
           :y2 (nth data (+ base 3))
           :score score
           :class-idx (long (nth data (+ base 5)))})))

(defn- decode-standard
  "Decodes the standard Ultralytics layout [1, 4+nc, anchors]: rows are
   cx, cy, w, h followed by one score per class."
  [data rows anchors confidence]
  (let [n-classes (- rows 4)]
    (into []
          (for [col (range anchors)
                :let [cx (nth data col)
                      cy (nth data (+ anchors col))
                      w (nth data (+ (* 2 anchors) col))
                      h (nth data (+ (* 3 anchors) col))
                      [class-idx best] (reduce
                                        (fn [[best-idx best-score] k]
                                          (let [s (nth data (+ (* (+ 4 k) anchors) col))]
                                            (if (> s best-score) [k s] [best-idx best-score])))
                                        [0 0.0]
                                        (range n-classes))]
                :when (>= best confidence)]
            {:x1 (- cx (/ w 2.0))
             :y1 (- cy (/ h 2.0))
             :x2 (+ cx (/ w 2.0))
             :y2 (+ cy (/ h 2.0))
             :score best
             :class-idx class-idx}))))

(defn decode-output
  "Decodes a YOLO detection output tensor. `data` is a flat indexed collection
   of floats (a vector), `dims` its shape. Supports the standard
   [1, 4+nc, anchors] layout and the NMS-in-graph [1, n, 6] layout. Returns maps
   with :x1 :y1 :x2 :y2 :score :class-idx in model-input pixel coordinates."
  ([data dims] (decode-output data dims default-confidence))
  ([data dims confidence]
   (let [[_ a b] dims]
     (if (== b 6)
       (decode-nms-layout data a confidence)
       (decode-standard data a b confidence)))))

(defn unletterbox
  "Maps a box from letterboxed model-input pixels back to original image pixels."
  [{:keys [x1 y1 x2 y2] :as box} {:keys [scale pad-x pad-y]}]
  (assoc box
         :x1 (/ (- x1 pad-x) scale)
         :y1 (/ (- y1 pad-y) scale)
         :x2 (/ (- x2 pad-x) scale)
         :y2 (/ (- y2 pad-y) scale)))

(defn box->detection
  "Converts a box to the detection map shape consumed by
   `riichi-calc.vision/parse-detections`, dropping unsupported classes."
  ([box] (box->detection class-labels box))
  ([labels {:keys [x1 y1 x2 y2 score class-idx]}]
   (when-let [label (class-index->label labels class-idx)]
     {:class label
      :confidence score
      :x (/ (+ x1 x2) 2.0)
      :y (/ (+ y1 y2) 2.0)
      :width (- x2 x1)
      :height (- y2 y1)})))

(defn postprocess
  "Full pipeline: tensor output (in model-input pixels) plus its letterbox
   geometry -> detections in original image coordinates, ready for
   `riichi-calc.vision/parse-detections`. `labels` in `opts` selects the class
   mapping (defaults to `class-labels`)."
  [data dims {:keys [confidence iou labels]} box]
  (let [labels (or labels class-labels)
        boxes (decode-output data dims (or confidence default-confidence))
        kept (nms boxes (or iou default-iou))]
    (->> kept
         (map #(unletterbox % box))
         (keep #(box->detection labels %))
         vec)))
