(ns riichi-calc.ui.reagent.reagentState
  (:require
   [riichi-calc.hand :as hand]
   [riichi-calc.state :as state]
   [riichi-calc.ui.reagent.audio :as audio]))


;; "Version of CommonState that plays a sound on update-hand"
(defrecord ReagentState [hand keyboard-mode theme language] 
  state/MutableRiichiState
  (update-hand [this path f tile]
    (audio/play-tile-down-sfx)
    (update-in this [:hand path] f tile)))

(def initial-state (->ReagentState (hand/hand) :an :regular :romaji))

(defn reset-initial! [*state]
  (reset! *state initial-state))

(defn remove-from-hand! [*state path index]
  (swap! *state update :hand hand/remove-from-hand path index))

(defn pon-conj! [*state tile]
  (swap! *state state/pon-conj tile))

(defn chii-conj! [*state tile]
  (swap! *state state/chii-conj tile))

(defn kan-conj! [*state tile]
  (swap! *state state/kan-conj tile))

(defn ankan-conj! [*state tile]
  (swap! *state state/ankan-conj tile))

(defn an-conj! [*state tile]
  (swap! *state state/an-conj tile))

(defn dorahyouji-conj! [*state tile]
  (swap! *state state/dorahyouji-conj tile))

(defn set-agaripai! [*state tile]
  (swap! *state state/set-agaripai tile))

(defn keyboard-input! [*state tile]
  (swap! *state state/keyboard-input tile))
