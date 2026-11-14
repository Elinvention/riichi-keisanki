(ns riichi-calc.dev
  "Development-only support for the shadow-cljs :dev-http server.

   Serves a live list of the ONNX models in resources/public/assets/models so
   the scanner's model picker stays up to date while `shadow-cljs watch app` is
   running, without having to edit index.html by hand."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [shadow.http.push-state :as push-state]))

(def ^:private models-endpoint "/assets/models/dev-models.json")

(def ^:private models-dir
  (io/file "resources/public/assets/models"))

(def ^:private qualifier-rank
  {"int8" 0, "int8s_conv" 0, "fp16" 1, "fp32" 2})

(defn- json-string [s]
  (str \"
       (-> (str s)
           (str/replace "\\" "\\\\")
           (str/replace "\"" "\\\""))
       \"))

(defn- model-sort-key
  "Sorts by newest version, then fastest quantisation, then name. Mirrors the
   ordering used by scripts/fingerprint.mjs so dev and release agree."
  [^java.io.File file]
  (let [name (.getName file)
        [_ version model] (re-matches #"riichi\.v(\d+)\.(.+)\.onnx" name)
        qualifier (if (and model (str/includes? model "."))
                    (last (str/split model #"\."))
                    "fp32")]
    [(- (if version (Long/parseLong version) 0))
     (get qualifier-rank qualifier 9)
     name]))

(defn- model-files []
  (when (.isDirectory models-dir)
    (->> (.listFiles models-dir)
         (filter #(str/ends-with? (.getName %) ".onnx"))
         (sort-by model-sort-key))))

(defn- model-json [^java.io.File file]
  (let [name (.getName file)]
    (str "{\"key\":" (json-string name)
         ",\"size\":" (.length file)
         ",\"url\":" (json-string (str "assets/models/" name)) "}")))

(defn- models-response []
  {:status 200
   :headers {"content-type" "application/json"
             "cache-control" "no-store"}
   :body (str "[" (str/join "," (map model-json (model-files))) "]")})

(defn handler
  "Fallback handler for the dev HTTP server (used when no static file matches).
   Responds to the live models manifest; everything else is delegated to
   shadow's push-state handler."
  [req]
  (if (= models-endpoint (:uri req))
    (models-response)
    (push-state/handle req)))
