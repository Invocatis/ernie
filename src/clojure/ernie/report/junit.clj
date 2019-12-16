(ns ernie.report.junit
  "A test reporter that outputs JUnit-compatible XML."
  (:require
    [ernie.util :refer :all]
    [clojure.data.xml :as xml]
    [clojure.string :as string]))

(defn report-var-element
  [{:keys [type] :as el}]
  (condp = type
    :fail (xml/element :failure (dissoc el :type))
    :error (xml/element :error (dissoc el :type))
    nil))

(defn report-var
  [var value]
  (xml/element :testcase
    {:name (-> var meta :name)
     :classname (-> var meta :ns ns-name name)}
    (report-var-element value)))

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

(defn report-ns
  [ns vars]
  (apply
    (partial xml/element :testsuite
             {:name (ns->suitename ns)
              :package (ns->packagename ns)})
    (map (partial apply report-var) (sort-by #(-> % meta :name) vars))))

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

(defn report
  [results]
  (let [results (clojure.walk/postwalk #(if (exception? %) (ex->str %) %) results)]
    (xml/emit-str
      (apply xml/element :testsuites {}
        (map (partial apply report-ns) results)))))
