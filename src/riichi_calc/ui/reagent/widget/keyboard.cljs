(ns riichi-calc.ui.reagent.widget.keyboard
  (:require
   [clojure.string :as s]
   [reagent.core :as r]
   [riichi-calc.group :as group]
   [riichi-calc.hand :as hand]
   [riichi-calc.tile :as tile]
   [riichi-calc.ui.reagent.reagentState :as state]
   [riichi-calc.ui.reagent.svg :as svg]
   [riichi-calc.ui.reagent.widget.base :as base]))

(defn- keyboard-key [*state update-state-fn! theme tile enabled]
  (let [plain-tile (svg/tile theme tile)]
    (if enabled
      (assoc-in plain-tile [1 :on-click] #(update-state-fn! *state tile))
      (update-in plain-tile [1 :style] assoc :opacity "50%"))))

(defn keyboard-mode [*state]
  [:fieldset#keyboard-mode.field [:legend "Keyboard mode:"]
   [base/radio-group "keyboard-mode"
                [:an :chii :pon :kan :ankan :dorahyouji :agaripai]
                (:keyboard-mode @*state)
                #(swap! *state assoc :keyboard-mode %1)]])

(defn keyboard [theme *state tiles enabled? update-state-fn! & children]
  (let [key-tiles (for [tile tiles
                        :let [enabled (enabled? tile)]]
                    ^{:key (str (svg/url theme tile) enabled)}
                    [(partial keyboard-key *state update-state-fn!) theme tile enabled])]
    (into [:div.field.keyboard
           (for [tile-row (partition 10 10 nil key-tiles)]
             ^{:key tile-row} [:span.tile-row tile-row])]
          children)))

(defn- from-notation-min [notation-min]
  (filter (some-fn group/tris? group/quad? group/straight?)
          (hand/grouped-tiles (hand/from-notation notation-min))))

(defn- cljs-copy-to-clipboard [text]
  (. (js/navigator.clipboard.writeText text) then
     #(println "Copying to clipboard was successful!")
     #(println "Could not copy text: " %1)))

(defn- copy-button [value]
  [:div.control
   [:input.button {:type :button
                   :name "notation-copy"
                   :value "Copy"
                   :onClick #(cljs-copy-to-clipboard value)
                   :disabled (empty? value)}]])

(defn notation [*state]
  (let [notation (r/atom "")
        typing (r/atom false)]
    (fn [*state]
      (let [{:keys [hand]} @*state
            [notation-an notation-min] (s/split @notation "|")
            hand-an (vec (hand/from-notation notation-an))
            hand-min (vec (from-notation-min notation-min))
            changed (or (not= (:an hand) hand-an) (not= (:min hand) hand-min))]
        (when changed
          (if @typing
            (do
              (swap! *state (fn [state]
                              (-> state
                                  (update :hand (fn [h] (assoc h :an hand-an :min hand-min)))
                                  (update :keyboard-mode (partial state/next-keyboard-mode state)))))
              (reset! typing false))
            (reset! notation (hand/to-notation hand)))))
      [:fieldset.field.has-addons
       [:legend "Notation"]
       [:div.control
        [:input.input {:type :text
                       :name "notation"
                       :value @notation
                       :onChange #(do (reset! notation (.. % -target -value)) (reset! typing true))}]]
       [copy-button @notation]])))

(defn main-keyboard [*state]
  (let [{kmode :keyboard-mode :keys [hand theme]} @*state
        enabled? (partial state/can-input? kmode hand)]
    [:div.field.flex-center
     [keyboard theme *state tile/all-34-tiles-with-redfives enabled? state/keyboard-input!]
     [keyboard-mode *state]
     [notation *state]]))
