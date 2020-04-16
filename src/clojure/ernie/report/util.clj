(ns ernie.report.util
  (:require
   [clojure.string :as string]
   [ernie.util :refer :all]))

(defn exception?
  [v]
  (instance? Exception v))

(defn ex->str
  [v]
  (let [s (stacktrace-string v)]
    (apply str s)))
      ; (take-while
      ;   (fn [s] (not (or (string/includes? s "ernie")
      ;                    (string/includes? s "clojure"))))
      ;   (string/split-lines s)))))

(defn ns->suitename
  [ns]
  (-> ns ns-name name
      (string/split #"\.")
      last))

(defn ns->packagename
  [ns]
  (->>
    (-> ns ns-name name
        (string/split #"\.")
        drop-last)
    (interpose ".")
    (apply str)))
