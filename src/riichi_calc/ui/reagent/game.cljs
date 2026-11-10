(ns riichi-calc.ui.reagent.game
  (:require
   [clojure.string :as s]
   [riichi-calc.game :as game]))

(defn player-actions [player-idx {:keys [winner looser agari on-tsumo on-ron on-ron-loser]}]
  (let [winner? (= player-idx winner)
        looser? (= player-idx looser)
        ron-pending? (and (some? winner) (nil? looser) (= :ron agari))]
    (cond
      winner?
      (if (= :tsumo agari)
        [:div.buttons.has-addons.is-centered
         [:button.button.is-success.is-small {:disabled true} "Tsumo W"]
         [:button.button.is-danger.is-small
          {:on-click #(on-ron player-idx)}
          "Ron +"]]
        [:div.buttons.has-addons.is-centered
         [:button.button.is-success.is-small
          {:on-click #(on-tsumo player-idx)}
          "Tsumo +"]
         [:button.button.is-danger.is-small {:disabled true} "Ron W"]])

      looser?
      [:div.buttons.has-addons.is-centered
       [:button.button.is-success.is-small
        {:on-click #(on-tsumo player-idx)}
        "Tsumo +"]
       [:button.button.is-danger.is-light.is-small {:disabled true} "Ron L"]]

      ron-pending?
      [:div.buttons.has-addons.is-centered
       [:button.button.is-danger.is-small
        {:on-click #(on-ron-loser player-idx)}
        "Ron −"]]

      :else
      [:div.buttons.has-addons.is-centered
       [:button.button.is-success.is-small
        {:on-click #(on-tsumo player-idx)}
        "Tsumo +"]
       [:button.button.is-danger.is-small
        {:on-click #(on-ron player-idx)}
        "Ron +"]])))

(defn player-score [wind player-idx player {:keys [on-name-change] :as actions}]
  [:div.level-item.has-text-centered
   [:div
    [:p.heading (s/capitalize (name wind))]
    [:div.field
     [:p.control
      [:input.input.is-small
       {:type "text"
        :value (:name player)
        :placeholder (str "Player " (inc player-idx))
        :style {:max-width "8rem" :margin "0 auto" :text-align "center"}
        :on-change #(on-name-change player-idx (.. % -target -value))}]]]
    [:p.title.mb-2 (:score player)]
    [player-actions player-idx actions]]])

(defn player-scores [players dealer actions]
  [:nav#player-scores.level
   (for [[player-idx player] (map-indexed vector players)]
     ^{:key (str "player-" player-idx)}
     [player-score (game/player-wind {:dealer dealer} player-idx) player-idx player actions])])