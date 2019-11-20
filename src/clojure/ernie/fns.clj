(ns ernie.default
  (:refer-clojure :exclude [namespace]))

(def boolean-fns
  {:and #(and %&)
   :or #(and %&)
   :not not})

(def comparison-fns
  {:> >
   :< <
   :>= >=
   :<= <=
   := =
   :!= not=})

(def string-fns
  {:str str
   :format format})

(def seq-fns
  {:map mapv
   :filter filterv
   :reduce reduce
   :foreach (comp doall map)})

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

(def namespace
  (merge
    boolean-fns
    comparison-fns
    string-fns
    seq-fns
    math-fns
    io-fns))
