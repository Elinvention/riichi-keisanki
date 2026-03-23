(ns riichi-calc.ui.reagent.wizard
  (:require
   [reagent.core :as r]
   [goog.string :as gstring]
   [riichi-calc.tile :as tile]
   [riichi-calc.hand :as hand]
   [riichi-calc.ui.reagent.reagentState :as state]
   [riichi-calc.ui.reagent.svg :as svg]
   [riichi-calc.ui.reagent.widget :as widget]))

(def initial-wizard {:open false :step 1})
(defonce *wizard (r/atom initial-wizard))

(defn close! []
  (swap! *wizard assoc :open false))

(defn open! []
  (swap! *wizard assoc :open true))

(defn toggle! []
  (swap! *wizard update :open not))

(defn restart! []
  (reset! *wizard initial-wizard))

(def ^:private steps
  [{:icon #(svg/tile-fg %1 (tile/wind (:jikaze %2))) :title "Jikaze"}
   {:icon #(svg/tile-fg %1 (tile/wind (:bakaze %2))) :title "Bakaze"}
   {:icon nil :title "Dora&shy;hyouji"}
   {:icon nil :title "Concealed tiles"}
   {:icon nil :title "Pon"}
   {:icon nil :title "Chii"}
   {:icon nil :title "Kan"}
   {:icon nil :title "AnKan"}
   {:icon nil :title "Agari"}])

(defn ^:private instance-step [theme hand step]
  (-> step
      (update :icon #(when % (% theme hand)))
      (update :title gstring/unescapeEntities)))

(defn ^:private render-steps [current-step {:keys [theme hand]}]
  [:ul.steps.has-content-centered.is-horizontal
   (let [steps-instanced (map (partial instance-step theme hand) steps)
         steps-indexed (map-indexed vector steps-instanced)]
     (for [[step {:keys [icon title]}] steps-indexed]
       ^{:key step}
       [:li.steps-segment {:class (when (= (inc step) current-step) "is-active")}
        [:span.steps-marker (when (some? icon) [:span.icon icon])]
        [:span.steps-content [:p {:style {:margin-top "8px"}} title]]]))])

(defn ^:private step-prev! []
  (swap! *wizard update :step (comp (partial max 1) dec)))

(defn ^:private step-next! []
  (swap! *wizard update :step (comp (partial min (count steps)) inc)))

(defn ^:private agari! [agari *state]
  (swap! *state assoc-in [:hand :agari] agari))

(def ^:private ron! (partial agari! :ron))

(def ^:private tsumo! (partial agari! :tsumo))

(defn ^:private render-nav [*state step]
  [:div.card-footer
   (when (> step 1) [:button.card-footer-item.button {:on-click step-prev!} "Previous step"])
   (if (< step (count steps))
     [:button.card-footer-item.button.is-primary
      {:on-click step-next!} "Next step"]
     [:<>
      [:button.card-footer-item.button.is-danger
       {:on-click (comp close! (partial ron! *state))} "Ron"]
      [:button.card-footer-item.button.is-success
       {:on-click (comp close! (partial tsumo! *state))} "Tsumo"]])])

(defn ^:private wind-keyboard [*state theme kaze]
  [widget/keyboard *state theme tile/wind-tiles (constantly true)
   #(do (swap! %1 assoc-in [:hand kaze] (:value %2)) (step-next!))])

(defn ^:private agaripai-keyboard [*state theme hand]
  [widget/keyboard *state theme tile/all-34-tiles-with-redfives
   (partial hand/can-agaripai? hand) state/set-agaripai!])

(defn ^:private pon-keyboard [*state theme hand]
  [widget/keyboard *state theme tile/all-34-tiles-with-redfives
   (partial hand/can-add-pon? hand) state/pon-conj!])

(defn ^:private chii-keyboard [*state theme hand]
  [widget/keyboard *state theme tile/all-34-tiles-with-redfives
   (partial hand/can-add-chii? hand) state/chii-conj!])

(defn ^:private ankan-keyboard [*state theme hand]
  [widget/keyboard *state theme tile/all-34-tiles-with-redfives
   (partial hand/can-add-kan? hand) state/ankan-conj!])

(defn ^:private kan-keyboard [*state theme hand]
  [widget/keyboard *state theme tile/all-34-tiles-with-redfives
   (partial hand/can-add-kan? hand) state/kan-conj!])

(defn ^:private closed-hand-keyboard [*state theme hand]
  [widget/keyboard *state theme tile/all-34-tiles-with-redfives
   (partial hand/can-add-tile? hand) state/an-conj!])

(defn ^:private dorahyouji-keyboard [*state theme hand]
  [widget/keyboard *state theme tile/all-34-tiles-with-redfives
   (partial hand/can-add-dorahyouji? hand) state/dorahyouji-conj!])

(defn render [*state]
  (let [{:keys [hand theme]} @*state
        remove-from-hand (partial state/remove-from-hand! *state)]
    (when (:open @*wizard)
      [:div#wizard.modal.is-active
       [:div.modal-background {:on-click close!}]
       [:div.modal-content
        [:div.card
         [:div.card-header [:p.card-header-title "Wizard"]]
         [:div.card-content
          [render-steps (:step @*wizard) @*state]]
         [:div.block.has-text-centered
          (case (:step @*wizard)
            1 [:div [:p "Please choose jikaze (sit wind)"] 
               [wind-keyboard *state theme :jikaze]]
            2 [:div [:p "Please choose bakaze (turn wind)"]
               [wind-keyboard *state theme :bakaze]]
            3 [:div [:p "Please choose dorahyouji (dora indicator)"]
               [dorahyouji-keyboard *state theme hand]
               [widget/dorahyouji *state hand]]
            4 [:div [:p "Please enter concealed tiles (anpai)"]
               [closed-hand-keyboard *state theme hand]
               [widget/hand-render theme hand remove-from-hand]]
            5 [:div [:p "Please enter pon"]
               [pon-keyboard *state theme hand]
               [widget/hand-render theme hand remove-from-hand]]
            6 [:div [:p "Please enter chii"]
               [chii-keyboard *state theme hand]
               [widget/hand-render theme hand remove-from-hand]]
            7 [:div [:p "Please enter kan"]
               [ankan-keyboard *state theme hand]
               [widget/hand-render theme hand remove-from-hand]]
            8 [:div [:p "Please enter concealed kan (ankan)"]
               [kan-keyboard *state theme hand]
               [widget/hand-render theme hand remove-from-hand]]
            9 [:div [:p "Please enter agaripai"]
               [agaripai-keyboard *state theme hand]
               [widget/agaripai-view *state (:agaripai hand)]]
            (swap! *state assoc-in [:wizard :step] 1))]
         [render-nav *state (:step @*wizard)]]]
       [:button.modal-close.is-large {:aria-label "close"
                                      :on-click close!}]])))
