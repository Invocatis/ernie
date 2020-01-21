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
   :or #(some identity %&)
   :not not})

(def comparison-fns
  {:less <
   :greater >
   :lessEq <=
   :greaterEq >=
   :eq =
   :neq not=})

(def string-fns
  {:str str
   :format format
   :match (fn [re s] (re-matches (java.util.regex.Pattern/compile re) s))
   :split (fn [re s] (clojure.string/split s (java.util.regex.Pattern/compile re)))
   :last-index-of clojure.string/last-index-of
   :substring subs})

(def seq-fns
  {:map mapv
   :filter filterv
   :reduce reduce
   :foreach (comp doall map)
   :first first
   :last last
   :nth nth
   :repeat repeat
   :repeatedly repeatedly
   :take take
   :drop drop
   :range range
   :pmap pmap
   :doall doall
   :count count})

(def math-fns
  {:+ +
   :- -
   :/ /
   :* *
   :mod mod})

(def io-fns
  {:println println
   :print print
   :read read})

(def assert-fns
  {:assert (fn [v & [msg]] (assert v msg))})

(def object-fns
  {:objectToEdn ernie.util/object->edn})

(def time-fns
  {:now #(.format (new java.text.SimpleDateFormat "MM-dd-yyyy__HH:mm:ss")
                  (new java.util.Date))})

(def set-fns
  {:set set
   :contains contains?
   :union set/union
   :intersection set/intersection
   :remove disj})

(def fn-fns
  {:partial partial})

(def parallel-fns
  {:parallel (fn [fs] (pmap #(apply % []) fs))})

(def namespace
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
