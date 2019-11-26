(ns ernie.core
  (:require
    [instaparse.core :as insta]
    [eftest.runner :as ef]
    [ernie.fns :as fns]
    [ernie.parser :refer [parse]]
    [ernie.semantics :as semantics]
    [ernie.log :as log]
    [ernie.util :refer :all]
    [ernie.report.junit :as junit]
    [clojure.pprint :refer [pprint]]))

(defn run
  [{:keys [namespace suites]} script]
  (let [expressions (parse script)]
    (if (insta/failure? expressions)
      (do (println expressions
           expressions))
      (semantics/eval|expressions
        (merge-with merge
                    {:cases fns/namespace} namespace)
        suites expressions))))

(defn suite-report-file-name
  [base-dir suite]
  (str base-dir "/"
    (clojure.string/replace (name (ns-name suite)) \. (.charAt java.io.File/separator 0))))

(defn normalize-results
  [results]
  (loop [results results
         joined {}
         count 0
         current-ns nil
         current-var nil
         summary {}]
    (if (empty? results)
      (with-meta joined {:count count :summary summary})
      (let [[{:keys [type] :as result} & results] results]
        (condp = type
          :begin-test-run (recur results joined
                                 count; (+ count (:count result))
                                 current-ns current-var summary)
          :begin-test-ns  (recur results (update joined (:ns result) #(or % {}))
                                 count (:ns result) current-var summary)
          :begin-test-var (recur results
                                 (update joined current-ns
                                         assoc (:var result) nil)
                                 count current-ns (:var result) summary)
          :end-test-ns    (recur results joined count nil nil summary)
          :summary        (recur results joined count current-ns current-var
                                 (merge-with + summary (dissoc result :type)))
          :error          (recur results
                                 (assoc-in joined [current-ns current-var]
                                           result)
                                 count current-ns current-var summary)
          :fail           (recur results
                                 (assoc-in joined [current-ns current-var]
                                           result)
                                 count current-ns current-var summary)
                          (recur results joined count
                                 current-ns current-var summary))))))

(defn results->report
  [results]
  (junit/report results))

(defn report
  [suites & [{:keys [report-dir]}]]
  (doseq [suite (vals suites)]
    (let [filename (when report-dir (suite-report-file-name report-dir suite))
          results (normalize-results @@(ns-resolve suite '_result))
          report (results->report results)]
      (when filename
        (clojure.java.io/make-parents filename))
      (spit (or (str filename ".xml") *out*) report))))
