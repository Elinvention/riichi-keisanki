(ns riichi-calc.ui.reagent.svg
  (:require
   [clojure.string :as s]
   [riichi-calc.tile :as tile]))

(def tile-width 48)
(def tile-height 64)

(defn url-from-name [theme tname]
  (str "assets/tiles/" (s/capitalize (name theme)) "/" tname ".svg"))

(defn url [theme tile]
  (url-from-name theme (tile/tile-name tile)))

(defn front-tile-bg [theme]
  [:image {:width tile-width :xlinkHref (url-from-name theme "Front")
           :on-drag-start #(.preventDefault %)}])

(defn front-tile-fg [theme tile]
  [:image {:xlinkHref (url theme tile)
           :width 40
           :transform "translate(4, 5)"
           :on-drag-start #(.preventDefault %)}])

(defn back-tile-bg [theme]
  [:image {:width tile-width :xlinkHref (url-from-name theme "Back")
           :on-drag-start #(.preventDefault %)}])

(defn front-tile [theme tile]
  [:g [front-tile-bg theme] [front-tile-fg theme tile]])

(def tile-container
  [:svg.mahjong-tile {:xmlns "http://www.w3.org/2000/svg"
                      :xmlnsXlink "http://www.w3.org/1999/xlink"
                      :view-box (str "0 0 " tile-width " " tile-height)}])

(defn tile [theme tile]
  (conj tile-container (if (some? tile) (front-tile theme tile) (back-tile-bg theme))))

(defn tile-rotated [theme wtile]
  [:span.rotated {} (tile theme wtile)])

(defn tile-fg [theme tile]
  (conj tile-container (front-tile-fg theme tile)))

