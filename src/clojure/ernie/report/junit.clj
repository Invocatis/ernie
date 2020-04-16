(ns ernie.report.junit
  "A test reporter that outputs JUnit-compatible XML."
  (:require
    [ernie.util :refer :all]
    [ernie.report.util :refer :all]
    [clojure.data.xml :as xml]
    [clojure.string :as string]))

(defn clean
  [el]
  (update el :message string/replace #"\n" "\\\\n"))

(defn report-var-element
  [{:keys [type] :as el}]
  (condp = type
    :fail (xml/element :failure (clean (dissoc el :type)))
    :error (xml/element :error (clean (dissoc el :type)))
    nil))

(defn report-var
  [var value]
  (xml/element :testcase
    {:name (-> var meta :name)
     :classname (-> var meta :ns ns-name name)}
    (report-var-element value)))

(defn report-ns
  [ns vars]
  (apply
    (partial xml/element :testsuite
             {:name (ns->suitename ns)
              :package (ns->packagename ns)})
    (map (partial apply report-var) (sort-by #(-> % meta :name) vars))))


(defn report
  [results]
  (let [results (clojure.walk/postwalk #(if (exception? %) (ex->str %) %) results)]
    (xml/emit-str
      (apply xml/element :testsuites {}
        (map (partial apply report-ns) results)))))
