(ns ernie.core
  (:require
    [instaparse.core :as insta]
    [ernie.parser :refer [parse]]
    [ernie.semantics :as semantics]
    [ernie.log :as log]
    [ernie.util :refer :all]
    [clojure.pprint :refer [pprint]])
  (:import
    [ernie.core Action Verify Clean]))

(defn run
  [namespace script]
  (let [expressions (parse script)]
    (if (insta/failure? expressions)
      (println expressions)
      (let [results (apply semantics/eval|expressions [namespace expressions])
            failures (->> results (filter failure?) (map :result))]
        (pprint failures)
        (if-not (empty? failures)
          (println (log/generate script failures))
          (println "Success!"))
        results))))
