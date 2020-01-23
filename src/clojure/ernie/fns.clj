(ns ernie.fns
  (:require
    [clojure.set :as set]
    [ernie.util])
  (:refer-clojure :exclude [namespace]))

(def control-flow-fns
  {:do #(do %&)
   :try #(try %1 %2)})

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
    parallel-fns))

(defn all-of-ns
  [ns]
  (let [public-vals (-> ns ns-publics vals)]
    (into {}
      (map (fn [v] [(-> v meta :name keyword) @v])
           (filter (fn [v] (fn? @v)) public-vals)))))
