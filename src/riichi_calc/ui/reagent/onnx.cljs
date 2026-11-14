(ns riichi-calc.ui.reagent.onnx
  "Thin wrapper around onnxruntime-web (loaded as the global `ort` via a script
   tag in index.html): loads the bundled YOLOv11 model once and runs detection on
   an <img> element, returning detections ready for
   `riichi-calc.vision/parse-detections`."
  (:require [reagent.core :as r]
            [riichi-calc.vision.yolo :as yolo]))

(defonce ^:private *sessions (atom {}))

(defn- ^js ort []
  (aget js/window "ort"))

(defn- configure! [^js o]
  ;; Resolve to an absolute URL: ORT loads its wasm factory with a dynamic
  ;; import(), which resolves relative to the compiled main.js (assets/js/), not
  ;; the page.
  (set! (.. o -env -wasm -wasmPaths)
        (.-href (js/URL. "assets/ort/" js/document.baseURI)))
  ;; Run the wasm in a dedicated worker (ORT's built-in proxy) so session
  ;; creation and inference don't block the UI thread.
  (set! (.. o -env -wasm -proxy) true)
  (set! (.. o -env -wasm -numThreads) 1))

(defn- fetch-model-data
  "Downloads `url`, reporting `(on-progress loaded-bytes total-bytes)` as it
   streams, and resolves to a Uint8Array of the whole file."
  [url on-progress]
  (-> (js/fetch url)
      (.then
       (fn [^js response]
         (when-not (.-ok response)
           (throw (js/Error. (str "Failed to download model (HTTP "
                                  (.-status response) ")"))))
         (let [total (js/parseInt (or (.get (.-headers response) "content-length") "0"))
               body (.-body response)]
           (if (and body (.-getReader body))
             (let [reader (.getReader body)
                   chunks (array)
                   loaded (volatile! 0)]
               (js/Promise.
                (fn [resolve reject]
                  (letfn [(pump []
                            (-> (.read reader)
                                (.then
                                 (fn [^js result]
                                   (if (.-done result)
                                     (let [out (js/Uint8Array. @loaded)]
                                       (loop [i 0 off 0]
                                         (when (< i (.-length chunks))
                                           (let [c (aget chunks i)]
                                             (.set out c off)
                                             (recur (inc i) (+ off (.-length c))))))
                                       (resolve out))
                                     (let [value (.-value result)]
                                       (.push chunks value)
                                       (vswap! loaded + (.-length value))
                                       (when on-progress (on-progress @loaded total))
                                       (pump)))))
                                (.catch reject)))]
                    (pump)))))
             (-> (.arrayBuffer response)
                 (.then (fn [buf]
                          (let [out (js/Uint8Array. buf)]
                            (when on-progress
                              (on-progress (.-length out) (.-length out)))
                            out))))))))))

(defn- normalize-model [m]
  {:key (:key m) :size (:size m) :url (:url m)})

(defn- injected-models []
  (if-let [ms (aget js/window "RIICHI_MODELS")]
    (mapv (fn [^js m]
            {:key (aget m "key") :size (aget m "size") :url (aget m "url")})
          (array-seq ms))
    [{:key "riichi.v2.yolo26s.int8.onnx"
      :url yolo/default-model-path}]))

(defonce ^:private *models (r/atom (injected-models)))

(defn models
  "Currently available detection models, each {:key :size :url}. In release
   builds this is the list baked into index.html (content-hashed by
   fingerprint.mjs); during development it is refreshed from the dev server so
   added/removed .onnx files show up without editing index.html."
  []
  @*models)

(defn refresh-models!
  "During development, fetches the live model list from the shadow-cljs dev
   server (see `riichi-calc.dev/handler`). No-op in release builds, where the
   list is baked into index.html."
  []
  (when ^boolean goog.DEBUG
    (-> (js/fetch "assets/models/dev-models.json" #js {:cache "no-store"})
        (.then (fn [^js response]
                 (if (.-ok response)
                   (.json response)
                   (throw (js/Error. "dev models manifest unavailable")))))
        (.then (fn [data]
                 (let [ms (mapv (fn [m] (normalize-model
                                         (js->clj m :keywordize-keys true)))
                                (array-seq data))]
                   (when (seq ms)
                     (reset! *models ms)))))
        (.catch (fn [err]
                  (js/console.debug "Live model list unavailable:" err))))))

(defn- default-model-path
  "The URL of the default model: the first entry of `models`, falling back to
   the bundled path for development."
  []
  (or (:url (first (models))) yolo/default-model-path))

(defn model-url
  "Resolves a model `key` (as persisted in settings) to its current URL, so a
   saved selection keeps working across content-hashed rebuilds."
  [key]
  (or (some (fn [m] (when (= key (:key m)) (:url m))) (models))
      (default-model-path)))

(def ^:private names-marker-length 5) ; "{0: '"

(defn- names-marker-at?
  "True when the `names` metadata marker (\"{0: '\") starts at `i` in `data`."
  [^js data i]
  (and (<= (+ i names-marker-length) (.-length data))
       (== (aget data i) 123)        ; {
       (== (aget data (+ i 1)) 48)   ; 0
       (== (aget data (+ i 2)) 58)   ; :
       (== (aget data (+ i 3)) 32)   ; space
       (== (aget data (+ i 4)) 39))) ; '

(defn- names-string
  "Finds the ONNX `names` metadata (stored as \"{0: 'label', ...}\") in the raw
   model bytes and returns it. It lives near the end of the file."
  [^js data]
  (let [from (loop [i (.indexOf data 123)] ; {
               (if (neg? i)
                 nil
                 (if (names-marker-at? data i)
                   i
                   (recur (.indexOf data 123 (inc i))))))]
    (when from
      (let [end (.indexOf data 125 from)] ; }
        (when (pos? end)
          (.decode (js/TextDecoder.) (.subarray data from (inc end))))))))

(defn- model-labels
  "Class labels declared in the model's ONNX metadata, so models trained with
   different class lists (e.g. v2 with red fives) decode correctly. Falls back
   to the v1 label list when the metadata is missing."
  [^js data]
  (if-let [labels (some-> (names-string data)
                          yolo/labels-from-names-string
                          not-empty)]
    labels
    (do (js/console.warn "No class names found in model metadata; using defaults.")
        yolo/class-labels)))

(defn- load-session!
  "Loads an InferenceSession for `path`, downloading the model the first time.
   Resolves to `{:session ... :labels ...}`, where `labels` are the class names
   declared by the model. `on-progress` is `(fn [loaded total])`."
  [^js o path on-progress]
  (if-let [cached (get @*sessions path)]
    cached
    (let [^js InferenceSession (.-InferenceSession o)
          promise (-> (fetch-model-data path on-progress)
                      (.then
                       (fn [data]
                         (let [labels (model-labels data)]
                           (-> (.create InferenceSession
                                        data
                                        #js {:executionProviders #js ["wasm"]
                                             :graphOptimizationLevel "all"})
                               (.then (fn [^js session]
                                        {:session session :labels labels}))))))
                      (.catch (fn [err]
                                (swap! *sessions dissoc path)
                                (throw err))))]
      (swap! *sessions assoc path promise)
      promise)))

(defn preload!
  "Starts downloading and preparing the model session for `path` ahead of time,
   so a later `run-detection!` reuses it. `on-progress` is `(fn [loaded total])`.
   Errors are swallowed (the session is evicted, so a run will retry)."
  [path on-progress]
  (when-let [o (ort)]
    (configure! o)
    (-> (load-session! o (or path (default-model-path)) on-progress)
        (.catch (fn [err]
                  (js/console.warn "Model preload failed:" err))))))

(defn- session-input-size [^js session]
  (let [metadata (.-inputMetadata session)
        ^js first-meta (when metadata (aget metadata 0))
        ^js shape (when first-meta (.-shape first-meta))
        w (when shape (aget shape 3))]
    (if (and (number? w) (pos? w)) w yolo/default-input-size)))

(defn- input-name [^js session]
  (or (aget (.-inputNames session) 0) "images"))

(defn- output-name [^js session]
  (or (aget (.-outputNames session) 0) "output0"))

(defn- typed-array->vec [ta]
  (let [n (.-length ta)]
    (loop [i 0 acc (transient [])]
      (if (< i n)
        (recur (inc i) (conj! acc (aget ta i)))
        (persistent! acc)))))

(defn- image->tensor-data
  "Letterboxes `img` into a size x size RGB canvas and returns a CHW
   Float32Array normalised to 0..1."
  [img size {:keys [draw-w draw-h pad-x pad-y]}]
  (let [canvas (js/document.createElement "canvas")]
    (set! (.-width canvas) size)
    (set! (.-height canvas) size)
    (let [ctx (.getContext canvas "2d")]
      (set! (.-fillStyle ctx) "rgb(114,114,114)")
      (.fillRect ctx 0 0 size size)
      (.drawImage ctx img 0 0 (.-width img) (.-height img) pad-x pad-y draw-w draw-h)
      (let [rgba (.-data (.getImageData ctx 0 0 size size))
            n (* size size)
            out (js/Float32Array. (* 3 n))]
        (dotimes [i n]
          (aset out i (/ (aget rgba (* i 4)) 255))
          (aset out (+ n i) (/ (aget rgba (inc (* i 4))) 255))
          (aset out (+ (* 2 n) i) (/ (aget rgba (+ (* i 4) 2)) 255)))
        out))))

(defn run-detection!
  "Runs the model on `img` (an HTMLImageElement) and returns a promise of
   `{:detections [...] :inference-ms n}` where the detections are in the original
   image coordinates and `:inference-ms` is the time spent in the model run.
   `on-progress`, when provided, is called with `(loaded-bytes total-bytes)`
   while downloading the model."
  [img {:keys [model-path confidence iou on-progress]}]
  (if-let [o (ort)]
    (do
      (configure! o)
      (let [Tensor (.-Tensor o)
            path (or model-path (default-model-path))]
        (-> (load-session! o path on-progress)
            (.then
             (fn [loaded]
               (let [^js session (:session loaded)
                     labels (:labels loaded)
                     size (session-input-size session)
                     box (yolo/letterbox (.-width img) (.-height img) size)
                     data (image->tensor-data img size box)
                     tensor (Tensor. "float32" data #js [1 3 size size])
                     feeds #js {}
                     started (js/Date.now)]
                 (aset feeds (input-name session) tensor)
                 (-> (.run session feeds)
                     (.then
                      (fn [^js results]
                        (let [elapsed (- (js/Date.now) started)
                              ^js out (aget results (output-name session))
                              flat (typed-array->vec (.-data out))
                              dims (typed-array->vec (.-dims out))
                              detections (yolo/postprocess flat dims
                                                           {:confidence confidence
                                                            :iou iou
                                                            :labels labels}
                                                           box)]
                          (.dispose out)
                          (.dispose tensor)
                          {:detections detections :inference-ms elapsed})))
                     (.catch (fn [err]
                               (.dispose tensor)
                               (throw err))))))))))
    (js/Promise.reject
     (js/Error. "ONNX Runtime failed to load (assets/ort/ort.wasm.min.js)"))))
