(ns ernie.fns
  (:require
    [ernie.util])
  (:refer-clojure :exclude [namespace]))

(def control-flow-fns
  {:do #(do %&)})

(def boolean-fns
  {:and #(and %&)
   :or #(and %&)
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
   :nth nth})

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
  {:now #(.format (new java.text.SimpleDateFormat "MM-dd-yyyy HH:mm:ss")
                  (new java.util.Date))})

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
    time-fns))
