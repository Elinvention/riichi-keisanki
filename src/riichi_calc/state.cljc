(ns riichi-calc.state
  (:require [clojure.core.match :refer [match]]
            [riichi-calc.group :as group]
            [riichi-calc.hand :as hand]
            [riichi-calc.tile :as tile]))

(def initial-state {:hand (hand/hand)
                    :keyboard-mode :an 
                    :theme :regular
                    :language :romaji
                    :wizard {:step 1 :open false}})

; TODO
(def play-sfx (atom nil))

(defn update-hand-with-sfx [play-sfx-fn state path f tile]
  (play-sfx-fn)
  (update-in state [:hand path] f tile))

(defn can-input? [keyboard-mode hand tile]
  (case keyboard-mode
    :an (hand/can-add-tile? hand tile)
    :chii (hand/can-add-chii? hand tile)
    :pon (hand/can-add-pon? hand tile)
    :kan (hand/can-add-kan? hand tile)
    :ankan (hand/can-add-kan? hand tile)
    :dorahyouji (hand/can-add-dorahyouji? hand tile)
    :agaripai (hand/can-agaripai? hand tile)))

(defn an-conj [update-fn {:keys [hand] :as state} tile]
  (if (hand/can-add-tile? hand tile)
    (update-fn state :an tile/conj-sort-tile tile)
    state))

(defn chii-conj [update-fn {:keys [hand] :as state} tile]
  (if (hand/can-add-chii? hand tile)
    (update-fn state :min tile/conj-sort-tile (group/straight tile))
    state))

(defn pon-conj [update-fn {:keys [hand] :as state} tile]
  (if (hand/can-add-pon? hand tile)
    (update-fn state :min tile/conj-sort-tile (group/tris tile))
    state))

(defn kan-conj [update-fn {:keys [hand] :as state} tile]
  (if (hand/can-add-kan? hand tile)
    (update-fn state :min tile/conj-sort-tile (group/quad tile))
    state))

(defn ankan-conj [update-fn {:keys [hand] :as state} tile]
  (if (hand/can-add-kan? hand tile)
    (update-fn state :an tile/conj-sort-tile (group/quad tile))
    state))


(defn dorahyouji-conj [update-fn {:keys [hand] :as state} tile]
  (if (hand/can-add-dorahyouji? hand tile)
    (update-fn state :dorahyouji conj tile)
    state))

(defn add-agaripai [state tile]
  (-> state
   (update-in [:hand :an] tile/conj-sort-tile tile)
   (assoc-in [:hand :agaripai] tile)))

(defn set-agaripai [{:keys [hand] :as state} tile]
  (cond
    (hand/can-add-tile? hand tile) (add-agaripai state tile)
    (and (= (hand/space-left hand) 0) (some #{tile} (hand/expand hand))) (assoc-in state [:hand :agaripai] tile)
    :else state))

(defn keyboard-input [update-fn {:keys [hand keyboard-mode] :as state} tile]
  (as-> state new-state
    (case keyboard-mode
      :an (an-conj update-fn state tile)
      :chii (chii-conj update-fn state tile)
      :pon (pon-conj update-fn state tile)
      :kan (kan-conj update-fn state tile)
      :ankan (ankan-conj update-fn state tile)
      :dorahyouji (dorahyouji-conj update-fn state tile)
      :agaripai (set-agaripai state tile)
      :else new-state)
    (let [space (hand/space-left (:hand new-state))
          agaripai (:agaripai (:hand new-state))
          next-kmode (match [space agaripai keyboard-mode]
                       [1 nil _] :agaripai
                       [0 nil _] :agaripai
                       [(_ :guard #(< % 3)) _ (:or :chii :pon :kan :ankan)] :an
                       :else keyboard-mode)]
      (assoc new-state :keyboard-mode next-kmode))))
