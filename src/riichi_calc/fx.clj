(ns riichi-calc.fx
  (:gen-class)
  (:require [cljfx.api :as fx]
            [clojure.string :refer [capitalize]]
            [riichi-calc.group :as group]
            [riichi-calc.hand :as hand]
            [riichi-calc.state :as state]
            [riichi-calc.tile :as tile]))

(def *state (atom state/initial-state))

(defn remove-from-hand [path index]
  (swap! *state update :hand hand/remove-from-hand path index))

(defn advance-wind [wind]
  (swap! *state update-in [:hand wind] tile/wind-next))


(defn url-from-name [theme tname]
  (str "file:resources/public/assets/tiles/Export/" (capitalize (name theme))
       "/" tname ".png"))

(defn url [theme tile]
  (url-from-name theme (tile/tile-name tile)))

(defn front-tile [{:keys [theme]}]
  {:fx/type :image-view
   :image {:url (url-from-name theme "Front")
           :requested-width 36
           :preserve-ratio true
           :background-loading true
           :smooth true}})

(defn back-tile [{:keys [theme]}]
  (assoc-in (front-tile {:theme theme}) [:image :url] (url-from-name theme "Back")))

(defn- tile-view [{:keys [tile on-mouse-clicked rotate bgcolor theme]
                   :or {on-mouse-clicked identity rotate 0 bgcolor "transparent"}}]
  (if (some? tile)
    {:fx/type :stack-pane
     :style {:-fx-background-color bgcolor}
     :on-mouse-clicked on-mouse-clicked
     :rotate rotate
     :children [{:fx/type front-tile
                 :theme theme}
                {:fx/type :image-view
                 :image {:url (url theme tile)
                         :requested-width 32
                         :preserve-ratio true
                         :background-loading true
                         :smooth true}}]}
    {:fx/type back-tile
     :theme theme}))

(defn agaripai-view [{:keys [tile theme]}]
  {:fx/type :v-box
   :spacing 2
   :children [{:fx/type :label :text "Agaripai"}
              {:fx/type tile-view
               :theme theme
               :tile tile
               :on-mouse-clicked {:event/type ::set-agaripai :tile nil}}]})

(defn an-view [{:keys [tile index theme]}]
  {:fx/type tile-view
   :theme theme
   :tile tile
   :on-mouse-clicked {:event/type ::remove-from-hand
                      :path :an
                      :index index}})

(defn min-view [{:keys [tile index rotate theme]}]
  {:fx/type tile-view
   :theme theme
   :tile tile
   :rotate rotate
   :on-mouse-clicked {:event/type ::remove-from-hand
                      :path :min
                      :index index}})

(defn dorahyouji-view [{:keys [tile index theme]}]
  {:fx/type tile-view
   :theme theme
   :tile tile
   :on-mouse-clicked {:event/type ::remove-from-hand
                      :path :dorahyouji
                      :index index}})

(defn- keyboard-key-button [{:keys [tile disable theme]}]
  {:fx/type :button
   :text ""
   :max-width 54
   :max-height 72
   :graphic {:fx/type tile-view :tile tile :theme theme}
   :on-action {:event/type ::keyboard-input :tile tile}
   :disable disable})

(defn radio-group [{:keys [options value on-action box]}]
  {:fx/type fx/ext-let-refs
   :refs {::toggle-group {:fx/type :toggle-group}} ;; define toggle group
   :desc {:fx/type box
          :spacing 10
          :children (for [option options]
                      {:fx/type :radio-button
                       ;; use previously defined toggle group
                       :toggle-group {:fx/type fx/ext-get-ref
                                      :ref ::toggle-group}
                       :selected (= option value)
                       :text (capitalize (name option))
                       :on-action (assoc on-action :option option)})}})

(defn- keyboard [{:keys [kmode akadora hand theme]}]
  {:fx/type :v-box
   :spacing 5
   :children [{:fx/type radio-group
               :box :h-box
               :options [:an :chii :pon :kan :ankan :dorahyouji :agaripai]
               :value kmode
               :on-action {:event/type ::set-keyboard-mode}}
              {:fx/type :check-box
               :text "Akadora (red five)"
               :selected akadora
               :on-selected-changed {:event/type ::set-akadora}}
              {:fx/type :tile-pane
               :pref-columns 9
               :hgap 1
               :vgap 1
               :pref-tile-width 54
               :pref-tile-height 72
               :children (for [t tile/all-34-tiles]
                           (let [actual-t (if (and akadora (= 5 (:value t)))
                                            (assoc t :red true)
                                            t)]
                             {:fx/type keyboard-key-button
                              :disable (not (state/can-input? kmode hand actual-t))
                              :tile actual-t
                              :theme theme}))}]})

(defn- hand-view [{:keys [hand theme]}]
  {:fx/type :tile-pane
   :pref-columns 14
   :hgap 1
   :vgap 1
   :pref-tile-width 54
   :pref-tile-height 72
   :min-height 72
   :style {:-fx-background-color :green}
   :children (concat
              (flatten
               (for [[index tile-or-group] (map-indexed vector (:an hand))]
                 (if (group/group? tile-or-group)
                   (for [[i tile] (map-indexed vector (group/expand tile-or-group))]
                     {:fx/type an-view :tile (when (< 0 i 3) tile) :index index :theme theme})
                   {:fx/type an-view :tile tile-or-group :index index :theme theme})))
              (for [[index group] (map-indexed vector (:min hand))
                    [i tile] (map-indexed vector (group/expand group))]
                {:fx/type min-view :tile tile :index index :rotate (if (= i 0) 90 0) :theme theme}))})

(defn- wind-button [{:keys [wind kind theme]}]
  {:fx/type :v-box
   :spacing 2
   :children [{:fx/type :label :text (capitalize (name kind))}
              {:fx/type :button
               :text ""
               :graphic {:fx/type tile-view :tile (tile/wind wind) :theme theme}
               :on-action {:event/type ::advance-wind :kind kind}
               :max-width 54
               :max-height 72}]})

(defn control-dorahyouji [{:keys [dorahyouji extra-yaku theme]}]
  {:fx/type :v-box
   :spacing 2
   :children [{:fx/type :label :text "Dorahyouji"}
              {:fx/type :tile-pane
               :hgap 2
               :children (let [n (if (contains? extra-yaku :riichi) 10 5)
                               doras (take n (lazy-cat dorahyouji (repeat nil)))]
                           (for [[index dora] (map-indexed vector doras)]
                             {:fx/type dorahyouji-view :tile dora :index index :theme theme}))}]})

(defn agari-view [{:keys [agari]}]
  {:fx/type :v-box
   :spacing 2
   :children [{:fx/type :label :text "Agari"}
              {:fx/type radio-group
               :box :v-box
               :options [:ron :tsumo]
               :value agari
               :on-action {:event/type ::set-agari}}]})

(defn control-extra-yaku [{:keys [extra-yaku]}]
  {:fx/type :v-box
   :spacing 2
   :children (cons {:fx/type :label :text "Extra Yaku"}
                   (for [yaku [:riichi :ippatsu :chankan :rinshan-kaihou :haitei-raoyue :houtei-raoyui]]
                     {:fx/type :check-box
                      :text (capitalize (name yaku))
                      :selected (contains? extra-yaku yaku)
                      :on-selected-changed {:event/type ::extra-yaku :yaku yaku}
                      :disable (and (= yaku :ippatsu) (not (contains? extra-yaku :riichi)))}))})

(defn control-theme [{:keys [theme]}]
  {:fx/type :v-box
   :spacing 2
   :children [{:fx/type :label
               :text "Tile Theme"}
              {:fx/type radio-group
               :options [:regular :black]
               :value theme
               :on-action {:event/type ::set-theme}
               :box :v-box}]})

(defn- control-buttons [{:keys [bakaze jikaze dorahyouji agari
                                agaripai extra-yaku theme] :as hand}]
  {:fx/type :h-box
   :spacing 10
   :children [{:fx/type wind-button
               :wind bakaze
               :kind :bakaze
               :theme theme}
              {:fx/type wind-button
               :wind jikaze
               :kind :jikaze
               :theme theme}
              {:fx/type control-dorahyouji
               :dorahyouji dorahyouji
               :extra-yaku extra-yaku
               :theme theme}
              {:fx/type agari-view
               :agari agari}
              {:fx/type agaripai-view
               :tile agaripai
               :theme theme}
              {:fx/type control-extra-yaku
               :extra-yaku extra-yaku}
              {:fx/type control-theme
               :theme theme}
              {:fx/type :button
               :text "Reset"
               :on-action {:event/type ::reset}
               :disable (= hand (:hand state/initial-state))}]})

(defn results-view [{:keys [results]}]
  {:fx/type :v-box
   :spacing 5
   :children [{:fx/type :label
               :style {:-fx-font-size 32}
               :text "Summary"}
              {:fx/type :label
               :text (:summary results)}]})

(def glossary
  {:fx/type :grid-pane
   :padding 5
   :hgap 20
   :alignment :top-left
   :children [{:fx/type :label
               :grid-pane/column 0
               :grid-pane/row 0
               :grid-pane/column-span 2
               :text "Glossary"
               :style {:-fx-font-size 32}}
              {:fx/type :label
               :grid-pane/column 0
               :grid-pane/row 2
               :text "Bakaze 場風 turn wind
Jikaze 自風 (own) seat wind
Dorahyoji ドラ表示 dora indicator
Agari 和がり generic call for winning a hand
Agaripai 和了り牌 winning tile
Ron 栄 win by deal in
Tsumo 自摸 win by self draw
An 暗 \"dark\" concealed tiles
Pon ポン call for open triplet
Chii チイ call for open straight
Kan 槓 call for open quad
Ankan 暗槓 call for concealed kan
Akadora 赤ドラ red fives"}
              {:fx/type :label
               :grid-pane/column 1
               :grid-pane/row 1
               :style {:-fx-font-size 24}
               :text "Yakus"}
              {:fx/type :label
               :grid-pane/column 1
               :grid-pane/row 2
               :text "Riichi 立直 special yaku
Ippatsu 一発 \"one-shot\" win with riichi in 1 turn
Chankan 搶槓 win with a tile stolen from an opponent's kan"}]})

(defn- root [{:keys [hand keyboard-mode akadora theme language]}]
  {:fx/type :stage
   :showing true
   :title "Riichi calculator"
   :scene {:fx/type :scene
           :root {:fx/type :scroll-pane
                  :fit-to-width true
                  :content {:fx/type :v-box
                            :fill-width true
                            :spacing 10
                            :children [(assoc hand :fx/type control-buttons :theme theme)
                                       {:fx/type hand-view
                                        :hand hand
                                        :theme theme}
                                       {:fx/type keyboard 
                                        :kmode keyboard-mode
                                        :akadora akadora
                                        :hand hand
                                        :theme theme}
                                       {:fx/type results-view
                                        :results (hand/results hand language)}
                                       glossary]}}}})

(def update-hand (partial state/update-hand-with-sfx #(println "TODO: play a sound!")))

(def keyboard-input (partial state/keyboard-input update-hand))

(defn map-event-handler [event]
  (case (:event/type event)
    ::set-keyboard-mode (swap! *state assoc :keyboard-mode (:option event))
    ::keyboard-input (swap! *state keyboard-input (:tile event))
    ::set-akadora (swap! *state assoc :akadora (:fx/event event))
    ::set-agari (swap! *state assoc-in [:hand :agari] (:option event))
    ::set-agaripai (swap! *state assoc-in [:hand :agaripai] (:tile event))
    ::advance-wind (advance-wind (:kind event))
    ::remove-from-hand (remove-from-hand (:path event) (:index event))
    ::set-theme (swap! *state assoc :theme (:option event))
    ::reset (reset! *state state/initial-state)
    ::extra-yaku (swap! *state update :hand #(if (:fx/event event)
                                               (hand/add-yaku % (:yaku event))
                                               (hand/remove-yaku % (:yaku event))))
    (println "unknown event:" event))
  (println @*state))

(def renderer
  (fx/create-renderer
   :middleware (fx/wrap-map-desc assoc :fx/type root)
   :opts {:fx.opt/map-event-handler map-event-handler}))

(defn -main [& _]
  (fx/mount-renderer *state renderer))

(-main)
