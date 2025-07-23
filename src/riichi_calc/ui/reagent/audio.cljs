(ns riichi-calc.ui.reagent.audio)

(defn play-tile-down-sfx []
  (let [domaudio (js/document.getElementById "klick4")]
    (set! (.-currentTime domaudio) 0)
    (-> (.play domaudio)
        (.catch #(println "Can't play sound:" (. % -message))))))

(def ^:private lang-to-iso
  {:romaji "jp-JP"
   :it "it-IT"
   :ja "ja-JP"
   :en "en-US"})

(defn ^:private positions
  "Returns a sequence of indices where the given predicate function returns
   true for the elements in the collection.
   
   `pred` is a predicate function that takes one argument (an element from `coll`)
    and returns a truthy or falsey value.
   `coll` is the collection to be iterated over.
  
   Examples:
   (positions even? [1 2 3 4 5]) ; => (1 3)
   (positions #(= \"a\" %) [\"b\" \"a\" \"c\" \"a\"]) ; => (1 3)
   (positions nil? [1 nil 2 nil 3]) ; => (1 3)"
  [pred coll]
  (keep-indexed (fn [idx x]
                  (when (pred x)
                    idx))
                coll))

(defn speak [lang text]
  (println lang text)
  (let [synth js/window.speechSynthesis
        voices (.getVoices synth)
        voice (get voices (first (positions #(#{(get lang-to-iso lang)} (.-lang %)) voices)))
        utterance (doto (js/SpeechSynthesisUtterance. text)
                    (set! -voice voice)
                    (set! -pitch 0))]
    (when (.-speaking synth) (.cancel synth))
    (when voice
      (println "Found voice " (.-lang voice))
      (.speak synth utterance))))
