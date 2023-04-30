(ns riichi-calc.state
  (:require [clojure.core.match :refer [match]]
            [riichi-calc.group :as group]
            [riichi-calc.hand :as hand]
            [riichi-calc.tile :as tile]))

(def initial-state {:hand (hand/hand)
                    :keyboard-mode :an})

(def *play-sfx-fn (atom nil))

(defn update-hand-with-sfx [state path f tile]
  (when-let [play @*play-sfx-fn] (play))
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

(defn next-keyboard-mode [{:keys [hand keyboard-mode] :as state}]
  (let [space (hand/space-left hand)
        agaripai (:agaripai hand)
        next-kmode (match [space agaripai keyboard-mode]
                [1 nil _] :agaripai
                [0 nil _] :agaripai
                [(_ :guard #(< % 3)) _ (:or :chii :pon :kan :ankan)] :an
                :else keyboard-mode)]
    (assoc state :keyboard-mode next-kmode)))

(defn an-conj [{:keys [hand] :as state} tile]
  (if (hand/can-add-tile? hand tile)
    (-> state
        (update-hand-with-sfx :an tile/conj-sort-tile tile)
        (next-keyboard-mode))
    state))

(defn atama-conj [{:keys [hand] :as state} tile]
  (if (hand/can-add-tile? hand tile 2)
    (-> state
        (update-hand-with-sfx :an (partial apply tile/conj-sort-tile) (tile/couple tile))
        (next-keyboard-mode))
    state))

(defn ankou-conj [{:keys [hand] :as state} tile]
  (if (hand/can-add-pon? hand tile)
    (-> state
        (update-hand-with-sfx :an (partial apply tile/conj-sort-tile) (tile/tris tile))
        (next-keyboard-mode))
    state))

(defn anjun-conj [{:keys [hand] :as state} tile]
  (if (hand/can-add-chii? hand tile)
    (-> state
        (update-hand-with-sfx :an (partial apply tile/conj-sort-tile) (tile/straight tile))
        (next-keyboard-mode))
    state))

(defn chii-conj [{:keys [hand] :as state} tile]
  (if (hand/can-add-chii? hand tile)
    (-> state
        (update-hand-with-sfx :min tile/conj-sort-tile (group/straight tile))
        (next-keyboard-mode))
    state))

(defn pon-conj [{:keys [hand] :as state} tile]
  (if (hand/can-add-pon? hand tile)
    (-> state
        (update-hand-with-sfx :min tile/conj-sort-tile (group/tris tile))
        (next-keyboard-mode))
    state))

(defn kan-conj [{:keys [hand] :as state} tile]
  (if (hand/can-add-kan? hand tile)
    (-> state
        (update-hand-with-sfx :min tile/conj-sort-tile (group/quad tile))
        (next-keyboard-mode))
    state))

(defn ankan-conj [{:keys [hand] :as state} tile]
  (if (hand/can-add-kan? hand tile)
    (-> state
        (update-hand-with-sfx :an tile/conj-sort-tile (group/quad tile))
        (next-keyboard-mode))
    state))

(defn dorahyouji-conj [{:keys [hand] :as state} tile]
  (if (hand/can-add-dorahyouji? hand tile)
    (-> state
        (update-hand-with-sfx :dorahyouji conj tile)
        (next-keyboard-mode))
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

(defn keyboard-input [{:keys [keyboard-mode] :as state} tile]
  (->
   (case keyboard-mode
     :an (an-conj state tile)
     :chii (chii-conj state tile)
     :pon (pon-conj state tile)
     :kan (kan-conj state tile)
     :ankan (ankan-conj state tile)
     :dorahyouji (dorahyouji-conj state tile)
     :agaripai (set-agaripai state tile)
     :else state)
   (next-keyboard-mode)))
