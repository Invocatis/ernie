(ns ernie.results.converters.junit
  (:require
    [clojure.data.xml :as xml :refer [element]]
    [clojure.walk :refer [stringify-keys keywordize-keys]]
    [clojure.pprint :refer [pprint]]
    [ernie.log :as log]
    [ernie.util :refer :all])
  (:import
    clojure.data.xml.Element))

(gen-class
  :name ernie.results.JUnit
  :prefix "-"
  :state state
  :init init
  :methods [^:static [convertResult
                      [java.lang.Object]
                      java.util.List]
            ^:static [bundleTestSuites
                      [java.util.Map java.util.List]
                      clojure.data.xml.Element]
            ^:static [xmlToString
                      [clojure.data.xml.Element]
                      String]])

(def ^:dynamic script)

(def timestamp-format
  (java.text.SimpleDateFormat. "yyyy-MM-dd'T'hh:mm:ss"))

(defn timestamp
  []
  (.format timestamp-format (java.util.Date.)))

(declare convert*)

(defn level->name
  [level]
  (->> level
    (map :attrs)
    (map #(get % "name"))
    (remove nil?)
    (interpose ".")
    (apply str)))

(defn add-case-to-suite
  [suite case]
  (update suite :content concat [case]))

(defn suite->testsuite
  [level {:keys [status result metadata time]}][]
  (element :testsuite (merge {:name (level->name (conj level {:attrs metadata})) :time time})))

(defn something->log
  [level {:keys [status expression] :as something}]
  (element :testcase
    {:name (or (log/line-source expression) expression)
     :classname (level->name level)
     :message (log/generate script something)}
    (when (= status :error)
      (element :error {} ()))
    (when (= status :failure)
      (element :failure {} ()))))

(defn convert-list
  [suite0 level result]
  (loop [suite suite0
         suites []
         result result]
    (if (empty? result)
      (into [suite] suites)
      (let [[suite & converted-suites] (convert* suite level (first result))]
        (recur suite (into suites converted-suites) (rest result))))))

(defn convert-suite
  [suite0 level {:keys [result] :as suite}]
  (let [level (conj level suite0)
        suite1 (suite->testsuite level suite)]
    (into [suite0] (convert* suite1 level result))))

(defn convert-scenario
  [suite level {:keys [status result metadata time] :as scenario}]
  (apply (partial convert* suite (conj level scenario) result)))

(defn convert-default
  [suite level {:keys [result] :as something}]
  (let [log (something->log level something)
        suite (add-case-to-suite suite log)]
    (convert* suite level result)))

(def mapping
  {:suite convert-suite
   :scenario convert-scenario})

(defn convert*
  [suite level result]
  (if (or (and (vector? result) (:external result))
          (and (map? result) (:external result))
          (not (or (vector? result) (map? result) (seq? result))))
    [suite]
    (if (empty? result)
      [suite]
      (if (or (seq? result) (vector? result))
        (convert-list suite level result)
        (let [{[op] :expression :keys [script type]} result]
          (if (and (nil? op) (nil? type))
            (convert* suite level (:result result))
            (if-let [f (get mapping (or type op))]
              (apply f [suite level result])
              (convert-default suite level result))))))))

(defn convert
  [result]
  (binding [script (get result :script)]
    (convert* (element :testsuite {:name ""}) [] result)))

(defn -convertResult
  [result]
  (convert result))

(defn -bundleTestSuites
  [attributes suites]
  (apply (partial element :testsuites attributes) suites))

(defn -xmlToString
  [xml]
  (xml/indent-str xml))
