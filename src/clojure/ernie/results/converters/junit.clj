(ns ernie.results.converters.junit
  (:require
    [clojure.data.xml :as xml]
    [ernie.util :refer :all]))


(defn convert|failure
  [{:keys [status verifcation result] :as test-case}]
  (xml/element (:message verification)
    {:message status :type (:status verification)}))

(defn convert|test-case
  [{:keys [status result details verification system-out system-err] :as test-case}]
  (element :testcase
    {:name (format "%s[%s]"
                   (:name details)
                   (apply str (interpose "," (:args details))))
     :status (:status verification)}
    (when (failure? test-case)
      (convert|failure test-case))
    (when system-out
      (element :system-out system-out))
    (when system-err
      (element :system-err system-err))))

(defn test-suite
  [])

(defn convert
  [test-suites]
  (apply (partial element :testsuites)))
