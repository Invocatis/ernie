(ns ernie.fns
  (:require
    [clojure.set :as set]
    [clojure.string :as string]
    [taoensso.timbre :as log]
    [ernie.util])
  (:refer-clojure :exclude [namespace]))

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

(def sensitive (atom #{}))

(defn mask-sensitive
  [s]
  (reduce (fn [acc sens] (string/replace acc sens (apply str (repeat (count sens) "*")))) s @sensitive))

(def log-fns
  {:log/info #(log/info (mask-sensitive %))
   :log/warn #(log/warn (mask-sensitive %))
   :log/error #(log/error (mask-sensitive %))
   :log/debug #(log/debug (mask-sensitive %))
   :log/infof #(log/infof (apply format (map mask-sensitive %&)))
   :log/warnf #(log/warnf (apply format (map mask-sensitive %&)))
   :log/errorf #(log/errorf (apply format (map mask-sensitive %&)))
   :log/debugf #(log/debugf (apply format (map mask-sensitive %&)))
   :log/level #(log/set-level! (keyword (mask-sensitive %)))
   :log/markSensitive #(swap! sensitive conj (mask-sensitive %))
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
    log-fns))

(defn all-of-ns
  [ns]
  (let [public-vals (-> ns ns-publics vals)]
    (into {}
      (map (fn [v] [(-> v meta :name keyword) @v])
           (filter (fn [v] (fn? @v)) public-vals)))))
