(ns riichi-calc.ui.reagent.history
  (:require
   [reagent.core :as r]
   [cljs.reader :as reader]
   [riichi-calc.tile :as tile]))

(def ^:private history-serialization-version 1)

(def ^:private initial-history
  {:version history-serialization-version
   :hands []})

(defn reader-read-tile [{:keys [seed value red]}]
  (tile/->Tile seed value red))

(reader/register-tag-parser! 'riichi-calc.tile.Tile reader-read-tile)

(defonce *history (r/atom []))

(defn ^:private parse-history-string [history-str]
  (try
    (reader/read-string history-str)
    (catch :default e
      (js/console.error "Error parsing history from local storage:" e)
      nil)))

(defn load-history
  "Loads history from local storage, handling parsing errors and versioning.
   Returns initial-history if no history is found, or if the stored history
   is unparseable or from a future version."
  []
  (if-let [stored-history-str (js/window.localStorage.getItem "history")]
    (if-let [parsed-history (parse-history-string stored-history-str)]
      (if-let [version (:version parsed-history)]
        (if (<= version history-serialization-version)
          parsed-history
          (do
            (js/console.warn "Stored history version is newer than expected. Using initial history.")
            initial-history))
        (do
          (js/console.warn "Stored history is missing version information. Using initial history.")
          initial-history))
      (do
        (js/console.warn "Failed to parse stored history. Using initial history.")
        initial-history))
    (do
      (js/console.info "No history found in local storage. Using initial history.")
      initial-history)))

(defn save-hand! [hand]
  (swap! *history update :hands conj hand))

(defn forget-hand! [i]
  (swap! *history (fn [history]
                    (update history :hands
                            (partial keep-indexed #(when (not= %1 i) %2))))))

(defn render [theme hand-render restore-hand!]
  (let [hands (take 10 (:hands @*history))
        history {:version history-serialization-version :hands hands}
        history-str (pr-str history)]
    (println "Saving history to localStorage as " history-str)
    (js/window.localStorage.setItem "history" history-str)
    [:div.field.content
     (if (empty? hands)
       [:div.notification.is-warning "History is empty."]
       [:<>
        [:button.button.is-danger {:on-click #(reset! *history [])} "Clear History"]
        [:ol (for [[i hand] (map-indexed vector hands)]
               ^{:key (str "history" i hand)}
               [:li [hand-render theme hand #(restore-hand! hand)]
                [:div.field.has-addons
                 [:div.control [:button.button.is-link {:on-click #(restore-hand! hand)} "Restore"]]
                 [:div.control [:button.button.is-danger {:on-click #(forget-hand! i)} "Forget"]]]])]])]))

(defn init! []
  (reset! *history (load-history)))
