(ns riichi-calc.ui.reagent.onnx
  "Thin wrapper around onnxruntime-web (loaded as the global `ort` via a script
   tag in index.html): loads the bundled YOLOv11 model once and runs detection on
   an <img> element, returning detections ready for
   `riichi-calc.vision/parse-detections`."
  (:require [riichi-calc.vision.yolo :as yolo]))

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

(defn- default-model-path
  "The model URL: injected into index.html at build time with a content hash
   (`window.RIICHI_MODEL_URL`), so it can be served with a long cache lifetime.
   Falls back to the plain path for development."
  []
  (or (aget js/window "RIICHI_MODEL_URL") yolo/default-model-path))

(defn models
  "Available detection models, injected into index.html at build time as
   `window.RIICHI_MODELS` (content-hashed in production). Each entry is
   {:key :name :size :url}. Falls back to the single bundled model in
   development."
  []
  (if-let [ms (aget js/window "RIICHI_MODELS")]
    (mapv (fn [^js m]
            {:key (aget m "key") :name (aget m "name")
             :size (aget m "size") :url (aget m "url")})
          (array-seq ms))
    [{:key "riichi.v1.yolo11m.onnx"
      :name "yolo11m"
      :url (default-model-path)}]))

(defn model-url
  "Resolves a model `key` (as persisted in settings) to its current URL, so a
   saved selection keeps working across content-hashed rebuilds."
  [key]
  (or (some (fn [m] (when (= key (:key m)) (:url m))) (models))
      (default-model-path)))

(defn- load-session!
  "Loads an InferenceSession for `path`, downloading the model the first time.
   `on-progress` is `(fn [loaded total])`."
  [^js o path on-progress]
  (if-let [cached (get @*sessions path)]
    cached
    (let [^js InferenceSession (.-InferenceSession o)
          promise (-> (fetch-model-data path on-progress)
                      (.then (fn [data]
                               (.create InferenceSession
                                        data
                                        #js {:executionProviders #js ["wasm"]
                                             :graphOptimizationLevel "all"})))
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
             (fn [^js session]
               (let [size (session-input-size session)
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
                                                            :iou iou}
                                                           box)]
                          (.dispose out)
                          (.dispose tensor)
                          {:detections detections :inference-ms elapsed})))
                     (.catch (fn [err]
                               (.dispose tensor)
                               (throw err))))))))))
    (js/Promise.reject
     (js/Error. "ONNX Runtime failed to load (assets/ort/ort.wasm.min.js)"))))
