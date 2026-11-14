(ns riichi-calc.vision
  "Maps object-detection output (mahjong tile classes) to riichi-calc tiles.
   Pure and platform-independent so it can be unit tested."
  (:require [riichi-calc.tile :as tile]))

(def default-confidence 0.4)

(def ^:private suffix-seeds
  {"B" :sou ;; Bamboo
   "C" :man ;; Characters
   "D" :pin ;; Dots
   })

(def ^:private honor-classes
  {"EW" [:wind :east]
   "SW" [:wind :south]
   "WW" [:wind :west]
   "NW" [:wind :north]
   "GD" [:dragon :green]
   "RD" [:dragon :red]
   "WD" [:dragon :white]})

(defn class->tile
  "Maps a detection class label such as \"1B\", \"EW\" or \"GD\" to a tile.
   Returns nil for unsupported classes (flowers, seasons, unknown)."
  [klass]
  (when (string? klass)
    (or (when-let [[_ value-str suffix] (re-matches #"([1-9])([BCD])" klass)]
          (tile/tile (suffix-seeds suffix) (parse-long value-str)))
        (when-let [[seed value] (honor-classes klass)]
          (tile/tile seed value)))))

(defn- reading-key
  "Sort key placing detections in reading order: left-to-right first (a riichi
   hand is normally a single horizontal row), then top-to-bottom as a
   tie-breaker."
  [{:keys [x y]}]
  [(or x 0) (or y 0)])

(defn parse-detections
  "Normalises detections into a vector of maps, each the original detection
   assoc'd with its :tile, keeping only supported classes above
   `min-confidence`, ordered in reading order."
  ([detections] (parse-detections detections default-confidence))
  ([detections min-confidence]
   (->> detections
        (filter #(>= (double (or (:confidence %) 0)) min-confidence))
        (keep #(when-let [t (class->tile (:class %))] (assoc % :tile t)))
        (sort-by reading-key)
        vec)))

(defn detections->tiles
  "Convenience wrapper returning just the tiles from `parse-detections`."
  ([detections] (detections->tiles detections default-confidence))
  ([detections min-confidence]
   (mapv :tile (parse-detections detections min-confidence))))
