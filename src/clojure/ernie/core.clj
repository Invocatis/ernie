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
      {:status :error
       :result expressions}
      (let [results (apply semantics/eval|expressions [namespace expressions])
            failures (->> results (filter failure?) (map :result))]
        (if-not (empty? failures)
          {:status :faiure
           :result {:script script
                    :failures failures}}
          {:status :success
           :result results})))))
