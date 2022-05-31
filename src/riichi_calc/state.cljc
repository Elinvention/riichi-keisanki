(ns riichi-calc.state
  (:require [clojure.core.match :refer [match]]
            [riichi-calc.group :as group]
            [riichi-calc.hand :as hand]
            [riichi-calc.tile :as tile]))

(def initial-state {:hand (hand/hand)
                    :keyboard-mode :an
                    :akadora false
                    :theme :regular
                    :language :romaji})


(defn can-input? [keyboard-mode akadora hand tile ukeire]
  (case keyboard-mode
    :an (hand/can-add-tile? hand tile)
    :chii (if akadora
            (hand/can-add-red-chii? hand tile)
            (hand/can-add-chii? hand tile))
    :pon (hand/can-add-pon? hand tile)
    :kan (hand/can-add-kan? hand tile)
    :ankan (hand/can-add-kan? hand tile)
    :dorahyouji (hand/can-add-dorahyouji? hand tile)
    :agaripai (if (> (hand/space-left hand) 0)
                (if (not-empty ukeire)
                  (contains? ukeire tile)
                  (hand/can-add-tile? hand tile))
                (some #{tile} (hand/expand-groups (:an hand))))))

(defn update-hand-with-sfx [play-sfx-fn state path f tile]
  (play-sfx-fn)
  (update-in state [:hand path] f tile))

(defn add-agaripai [state tile]
  (as-> state new-state
    (update-in new-state [:hand :an] tile/conj-sort-tile tile)
    (assoc-in new-state [:hand :agaripai] tile)))

(defn keyboard-input [{:keys [hand keyboard-mode akadora] :as state} tile update-fn]
  (as-> state new-state
    (match [keyboard-mode akadora tile]
      [:an _ (_ :guard (partial hand/can-add-tile? hand))]
      (update-fn new-state :an tile/conj-sort-tile tile)
      [:chii true (_ :guard (partial hand/can-add-red-chii? hand))]
      (if-let [red-straight (group/red-straight tile)]
        (update-fn new-state :min conj red-straight)
        new-state)
      [:chii false (_ :guard (partial hand/can-add-chii? hand))]
      (if-let [straight (group/straight tile)]
        (update-fn new-state :min conj straight)
        new-state)
      [:pon _ (_ :guard (partial hand/can-add-pon? hand))]
      (update-fn new-state :min conj (group/tris tile))
      [:kan _ (_ :guard (partial hand/can-add-kan? hand))]
      (update-fn new-state :min conj (group/quad tile))
      [:ankan _ (_ :guard (partial hand/can-add-kan? hand))]
      (update-fn new-state :an conj (group/quad tile))
      [:dorahyouji _ (_ :guard (partial hand/can-add-dorahyouji? hand))]
      (update-fn new-state :dorahyouji conj tile)
      [:agaripai _ (_ :guard #(hand/can-add-tile? hand %))]
      (add-agaripai new-state tile)
      [:agaripai _ (_ :guard #(and (= (hand/space-left hand) 0) (some #{%} (hand/expand hand))))]
      (assoc-in new-state [:hand :agaripai] tile)
      :else new-state)
    (let [space (hand/space-left (:hand new-state))
          agaripai (:agaripai (:hand new-state))
          next-kmode (match [space agaripai keyboard-mode]
                       [1 nil _] :agaripai
                       [0 nil _] :agaripai
                       [(_ :guard #(< % 3)) _ (:or :chii :pon :kan :ankan)] :an
                       :else keyboard-mode)]
      (assoc new-state :keyboard-mode next-kmode))))
