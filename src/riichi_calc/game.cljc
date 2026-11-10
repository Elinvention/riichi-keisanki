(ns riichi-calc.game
  (:require [riichi-calc.tile :as tile]))

(def initial-state
  {:players [{:name "Player 1" :score 25000}
             {:name "Player 2" :score 25000}
             {:name "Player 3" :score 25000}
             {:name "Player 4" :score 25000}]
   :current-hand nil
   :round {:wind :east :number 1}
   :dealer 0
   :honba 0})

(defn player-wind [state player-idx]
  (let [dealer (or (:dealer state) 0)]
    (nth tile/winds (mod (- player-idx dealer) 4))))

(defn next-dealer [state winner-idx]
  (let [dealer (or (:dealer state) 0)]
    (if (= winner-idx dealer)
      dealer
      (mod (inc dealer) 4))))

(defn- get-score [hand]
  (cond
    (:score hand) (:score hand)
    (:total hand) hand
    :else nil))

(defn update-scores [state winner-fn]
  (let [score (get-score (:current-hand state))]
    (winner-fn state (:total score))))

(defn ron [state winner-idx looser-idx]
  (update-scores state
                 (fn [st score]
                   (-> st
                       (update-in [:players winner-idx :score] (fnil + 0) score)
                       (update-in [:players looser-idx :score] (fnil - 0) score)))))

(defn- wind-to-idx [wind]
  (case wind :east 0 :south 1 :west 2 :north 3 0))

(defn tsumo [state winner-idx]
  (let [hand-score (get-score (:current-hand state))
        winner-jikaze (:jikaze hand-score)
        dealer-idx (mod (- winner-idx (wind-to-idx winner-jikaze)) 4)
        dealer? (= winner-idx dealer-idx)
        payments (if dealer?
                   (let [payment (get-in hand-score [:split :everyone-pay])]
                     (for [p-idx (range (count (:players state)))]
                       (if (= p-idx winner-idx)
                         (* 3 (or payment 0))
                         (- (or payment 0)))))
                   (let [dealer-pay (get-in hand-score [:split :dealer-pay])
                         non-dealer-pay (get-in hand-score [:split :non-dealer-pay])]
                     (for [p-idx (range (count (:players state)))]
                       (cond
                         (= p-idx winner-idx) (+ (or dealer-pay 0) (* 2 (or non-dealer-pay 0)))
                         (= p-idx dealer-idx) (- (or dealer-pay 0))
                         :else (- (or non-dealer-pay 0))))))]
    (update-in state [:players] (fn [players]
                                  (vec (map (fn [player payment]
                                              (update player :score (fnil + 0) payment))
                                            players
                                            payments))))))

(defn reset-match [state]
  (assoc state :players (:players initial-state)
               :round (:round initial-state)
               :dealer (:dealer initial-state)
               :honba (:honba initial-state)))
