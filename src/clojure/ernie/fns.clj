(ns ernie.fns
  (:require
    [clojure.set :as set]
    [clojure.string :as string]
    [ernie.util]
    [ernie.logger :as log]
    [ernie.semantics :as s])
  (:refer-clojure :exclude [namespace]))

(def bootstrap-fns
  {:if ()})

(def control-flow-fns
  {:do #(do %&)})

(def boolean-fns
  {:and #(if (every? identity %&) (last %&) (first (filter (complement identity) %&)))
   :or #(some identity %&)})

(def comparison-fns
  {})

(def string-fns
  {:match (fn [re s] (re-matches (java.util.regex.Pattern/compile re) s))
   :split (fn [re s] (clojure.string/split s (java.util.regex.Pattern/compile re)))
   :substring subs})

(def seq-fns
  {:map mapv
   :filter filterv
   :reduce reduce
   :foreach (comp doall map)})

(def math-fns
  {})

(def io-fns
  {})

(def assert-fns
  {})

(def object-fns
  {:objectToEdn ernie.util/object->edn})

(def time-fns
  {:now #(.format (new java.text.SimpleDateFormat "MM-dd-yyyy__HH:mm:ss")
                  (new java.util.Date))})

(def set-fns
  {})

(def fn-fns
  {})

(def parallel-fns
  {:parallel (fn [fs] (pmap #(apply % []) fs))})

(def log-fns
  {:log/info log/-info
   :log/warn log/-warn
   :log/error log/-error
   :log/debug log/-debug
   :log/infof (comp log/-info format)
   :log/warnf (comp log/-warn format)
   :log/errorf (comp log/-error format)
   :log/debugf (comp log/-debug format)
   :log/level log/set-level!
   :log/markSensitive log/mark-sensitive
   :log/appendToConsole #(if %
                          (log/swap-config! assoc-in [:appenders :println :enabled] true)
                          (log/swap-config! assoc-in [:appenders :println :enabled] false))
   :log/appendToFile #(do
                       (spit % nil)
                       (log/swap-config! assoc-in [:appenders (keyword (str "file-" %))]
                                         {:enabled? true
                                          :async? false
                                          :min-level nil
                                          :rate-limit nil
                                          :output-fn :inherit,
                                          :fn (fn [{:keys [msg_]}] (spit % (str @msg_ \newline) :append true))}))})


(def shell-fns
  {:shell/exec #(let [proc (.exec (Runtime/getRuntime) %)
                      in (slurp (.getInputStream proc))
                      err (slurp (.getErrorStream proc))]
                  (if (or (empty? in) (empty? err))
                    (str in err)
                    (str in \newline err)))
   :shell/var #(System/getenv %)})

(def namespace*
  (merge
    control-flow-fns
    boolean-fns
    comparison-fns
    string-fns
    seq-fns
    math-fns
    io-fns
    assert-fns
    object-fns
    time-fns
    set-fns
    fn-fns
    parallel-fns
    log-fns
    shell-fns))

(defn all-of-ns
  [ns & [as]]
  (let [public-vals (-> ns ns-publics vals)]
    (into {}
      (map (fn [v] [(if as (->> v meta :name (str as "/") keyword) (-> v meta :name keyword)) @v])
           (filter (fn [v] (fn? @v)) public-vals)))))
