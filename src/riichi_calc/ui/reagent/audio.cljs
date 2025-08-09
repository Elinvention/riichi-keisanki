(ns riichi-calc.ui.reagent.audio)

;; Constants
(def ^:private audio-element-id "klick4")
(def ^:private default-pitch 1.0)
(def ^:private default-rate 1.0)
(def ^:private default-volume 1.0)

(def ^:private lang-to-iso
  "Mapping of application language keys to ISO language codes for speech synthesis."
  {:romaji "ja-JP"  ; Romaji is Japanese romanization, so use Japanese voice
   :it "it-IT"
   :ja "ja-JP"
   :en "en-US"})

;; Utility functions

(defn ^:private find-voice-by-lang
  "Finds the first available voice that matches the given language code.
   Returns nil if no matching voice is found."
  [^js voices ^string target-lang]
  (when (and voices target-lang)
    (->> voices
         (filter #(= target-lang (.-lang %)))
         first)))

(defn ^:private create-utterance
  "Creates a SpeechSynthesisUtterance with the given text and optional configuration."
  [^string text {:keys [voice pitch rate volume]
                 :or {pitch default-pitch
                      rate default-rate
                      volume default-volume}}]
  (doto (js/SpeechSynthesisUtterance. text)
    (set! -voice voice)
    (set! -pitch pitch)
    (set! -rate rate)
    (set! -volume volume)))

;; Public API
(defn play-tile-down-sfx
  "Plays the tile placement sound effect. Resets the audio to the beginning
   before playing to ensure it can be played multiple times in quick succession."
  []
  (when-let [audio-element (js/document.getElementById audio-element-id)]
    (try
      (set! (.-currentTime audio-element) 0)
      (-> (.play audio-element)
          (.catch #(js/console.warn "Failed to play sound effect:" (.-message %))))
      (catch js/Error e
        (js/console.error "Error accessing audio element:" (.-message e))))))

(defn speak
  "Speaks the given text using the browser's speech synthesis API.
   
   Parameters:
   - `lang`: Language key (from lang-to-iso mapping)
   - `text`: Text to be spoken
   - `options`: Optional map with :pitch, :rate, :volume keys
   
   If a voice for the specified language is not available, falls back to
   the default system voice."
  ([^keyword lang ^string text]
   (speak lang text {}))
  ([^keyword lang ^string text options]
   (when (and lang text (not-empty text))
     (try
       (let [synth js/window.speechSynthesis]
         (when synth
           ;; Cancel any ongoing speech
           (when (.-speaking synth)
             (.cancel synth))
           
           (let [voices (.getVoices synth)
                 target-lang (get lang-to-iso lang)
                 voice (find-voice-by-lang voices target-lang)
                 utterance (create-utterance text (assoc options :voice voice))]
             
             (if voice
               (do
                 (js/console.log "Using voice:" (.-name voice) "(" (.-lang voice) ")")
                 (.speak synth utterance))
               (do
                 (js/console.warn "No voice found for language:" target-lang "- using default voice")
                 (.speak synth utterance))))))
       (catch js/Error e
         (js/console.error "Speech synthesis error:" (.-message e)))))))
