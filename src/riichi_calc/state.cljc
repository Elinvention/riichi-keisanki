(ns riichi-calc.state
  (:require [clojure.core.match :refer [match]]
            [riichi-calc.group :as group]
            [riichi-calc.hand :as hand]
            [riichi-calc.tile :as tile]))

(defprotocol MutableRiichiState
  "Modify a State"
  (update-hand [this path f tile]))

(defrecord CommonState [hand keyboard-mode theme language]
  MutableRiichiState
  (update-hand [this path f tile]
    (update-in this [:hand path] f tile)))

(def initial-state (->CommonState (hand/hand) :an :regular :romaji))

(defn can-input? [keyboard-mode hand tile]
  (case keyboard-mode
    :an (hand/can-add-tile? hand tile)
    :chii (hand/can-add-chii? hand tile)
    :pon (hand/can-add-pon? hand tile)
    :kan (hand/can-add-kan? hand tile)
    :ankan (hand/can-add-kan? hand tile)
    :dorahyouji (hand/can-add-dorahyouji? hand tile)
    :agaripai (hand/can-agaripai? hand tile)))

(defn an-conj [{:keys [hand] :as state} tile]
  (if (hand/can-add-tile? hand tile)
    (update-hand state :an tile/conj-sort-tile tile)
    state))

(defn chii-conj [{:keys [hand] :as state} tile]
  (if (hand/can-add-chii? hand tile)
    (update-hand state :min tile/conj-sort-tile (group/straight tile))
    state))

(defn pon-conj [{:keys [hand] :as state} tile]
  (if (hand/can-add-pon? hand tile)
    (update-hand state :min tile/conj-sort-tile (group/tris tile))
    state))

(defn kan-conj [{:keys [hand] :as state} tile]
  (if (hand/can-add-kan? hand tile)
    (update-hand state :min tile/conj-sort-tile (group/quad tile))
    state))

(defn ankan-conj [{:keys [hand] :as state} tile]
  (if (hand/can-add-kan? hand tile)
    (update-hand state :an tile/conj-sort-tile (group/quad tile))
    state))


(defn dorahyouji-conj [{:keys [hand] :as state} tile]
  (if (hand/can-add-dorahyouji? hand tile)
    (update-hand state :dorahyouji conj tile)
    state))

(defn add-agaripai [state tile]
  (-> state
   (update-hand :an tile/conj-sort-tile tile)
   (assoc-in [:hand :agaripai] tile)))

(defn set-agaripai [{:keys [hand] :as state} tile]
  (cond
    (hand/can-add-tile? hand tile) (add-agaripai state tile)
    (and (= (hand/space-left hand) 0) (some #{tile} (hand/expand hand))) (assoc-in state [:hand :agaripai] tile)
    :else state))

(defn next-keyboard-mode [{:keys [hand]} keyboard-mode]
  (let [space (hand/space-left hand)
        agaripai (:agaripai hand)]
    (match [space agaripai keyboard-mode]
      [1 nil _] :agaripai
      [0 nil _] :agaripai
      [(_ :guard #(< % 3)) _ (:or :chii :pon :kan :ankan)] :an
      :else keyboard-mode)))

(defn keyboard-input [{:keys [keyboard-mode] :as state} tile]
  (as-> state new-state
    (case keyboard-mode
      :an (an-conj state tile)
      :chii (chii-conj state tile)
      :pon (pon-conj state tile)
      :kan (kan-conj state tile)
      :ankan (ankan-conj state tile)
      :dorahyouji (dorahyouji-conj state tile)
      :agaripai (set-agaripai state tile)
      :else new-state)
    (update new-state :keyboard-mode (partial next-keyboard-mode new-state))))

(defn set-extra-dora! [*state dora]
  (swap! *state assoc-in [:hand :extra :dora] dora))

(defn set-extra-yaku! [*state yaku]
  (swap! *state assoc-in [:hand :extra :yaku] yaku))
