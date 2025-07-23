(ns riichi-calc.ui.reagent.wizard
  (:require
   [reagent.core :as r]
   [goog.string :as gstring]
   [riichi-calc.tile :as tile]
   [riichi-calc.hand :as hand]
   [riichi-calc.ui.reagent.reagentState :as state]
   [riichi-calc.ui.reagent.svg :as svg]
   [riichi-calc.ui.reagent.widget :as widget]))

(defonce *wizard (r/atom {:open false :step 1}))

(defn close! []
  (swap! *wizard assoc :open false))

(defn open! []
  (swap! *wizard assoc :open true))

(defn toggle! []
  (swap! *wizard update :open not))

(defn ^:private steps [current-step {:keys [theme hand]}]
  [:ul.steps.has-content-centered.is-horizontal
   (let [steps [{:icon (svg/tile-fg theme (tile/wind (:jikaze hand))) :title "Jikaze"}
                {:icon (svg/tile-fg theme (tile/wind (:bakaze hand))) :title "Bakaze"}
                {:icon nil :title (gstring/unescapeEntities "Dora&shy;hyouji")}
                {:icon nil :title "Closed hand"}
                {:icon nil :title "Open hand"}
                {:icon nil :title "Agari"}]
         steps-indexed (map-indexed vector steps)]
     (for [[step {:keys [icon title]}] steps-indexed]
       ^{:key step}
       [:li.steps-segment {:class (when (= (inc step) current-step) "is-active")}
        [:span.steps-marker (when (some? icon) [:span.icon icon])]
        [:span.steps-content [:p {:style {:margin-top "8px"}} title]]]))])

(defn ^:private step-prev! []
  (swap! *wizard update :step (comp (partial max 1) dec)))

(defn ^:private step-next! []
  (swap! *wizard update :step (comp (partial min 6) inc)))

(defn ^:private agari! [agari *state]
  (swap! *state assoc-in [:hand :agari] agari))

(def ^:private ron! (partial agari! :ron))

(def ^:private tsumo! (partial agari! :tsumo))

(defn ^:private nav [*state step]
  [:div.card-footer
   (when (> step 1) [:button.card-footer-item.button {:on-click step-prev!} "Previous step"])
   (if (< step 6)
     [:button.card-footer-item.button.is-primary
      {:on-click step-next!} "Next step"]
     [:<>
      [:button.card-footer-item.button.is-danger
       {:on-click (comp close! (partial ron! *state))} "Ron"]
      [:button.card-footer-item.button.is-success
       {:on-click (comp close! (partial tsumo! *state))} "Tsumo"]])])

(defn ^:private wind-keyboard [*state theme kaze]
  [widget/keyboard *state theme tile/wind-tiles (constantly true)
   #(swap! %1 assoc-in [:hand kaze] (:value %2))])

(defn ^:private agaripai-keyboard [*state theme hand]
  [widget/keyboard *state theme tile/all-34-tiles-with-redfives
   (partial hand/can-agaripai? hand) state/set-agaripai!])

(defn ^:private open-hand-keyboard [*state theme hand]
  [widget/keyboard *state theme tile/all-34-tiles-with-redfives
   (partial hand/can-add-pon? hand) state/pon-conj!])

(defn ^:private closed-hand-keyboard [*state theme hand]
  [widget/keyboard *state theme tile/all-34-tiles-with-redfives
   (partial hand/can-add-tile? hand) state/an-conj!])

(defn ^:private dorahyouji-keyboard [*state theme hand]
  [widget/keyboard *state theme tile/all-34-tiles-with-redfives
   (partial hand/can-add-dorahyouji? hand) state/dorahyouji-conj!])

(defn render [*state]
  (let [{:keys [hand theme]} @*state]
    [(if (:open @*wizard) :div#wizard.modal.is-active :div#wizard.modal)
     [:div.modal-background {:on-click #(swap! *state assoc-in [:wizard :open] false)}]
     [:div.modal-content
      [:div.card
       [:div.card-header [:p.card-header-title "Wizard"]]
       [:div.card-content
        [steps (:step @*wizard) @*state]]
       [:div.block.has-text-centered
        (case (:step @*wizard)
          1 [:div [:p "Please choose jikaze"] 
             [wind-keyboard *state theme :jikaze]]
          2 [:div [:p "Please choose bakaze"]
             [wind-keyboard *state theme :bakaze]]
          3 [:div [:p "Please choose dorahyouji"]
             [dorahyouji-keyboard *state theme hand]
             [widget/dorahyouji *state hand]]
          4 [:div [:p "Please enter closed hand"]
             [closed-hand-keyboard *state theme hand]
             [widget/hand-render theme hand (partial state/remove-from-hand! *state)]]
          5 [:div [:p "Please enter open hand"]
             [open-hand-keyboard *state theme hand]
             [widget/hand-render theme hand (partial state/remove-from-hand! *state)]]
          6 [:div [:p "Please enter agaripai"]
             [agaripai-keyboard *state theme hand]
             [widget/agaripai-view *state (:agaripai hand)]]
          (swap! *state assoc-in [:wizard :step] 1))]
       [nav *state (:step @*wizard)]]]
     [:button.modal-close.is-large {:aria-label "close"
                                    :on-click close!}]]))
