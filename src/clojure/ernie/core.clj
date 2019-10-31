(ns ernie.core
  (:require
    [instaparse.core :as insta]
    [ernie.parser :refer [parse]]
    [ernie.semantics :as semantics]
    [ernie.log :as log]
    [ernie.util :refer :all]
    [clojure.pprint :refer [pprint]]))

(defn run
  [namespace script]
  (let [expressions (parse script)]
    (if (insta/failure? expressions)
      (do (println expressions)
        ^:internal
        {:status :error
         :script script
         :result [{:status :failure
                   :result [:error :syntax expressions]}]})
      (let [results (semantics/eval|expressions namespace expressions)
            failures (->> results (filter failure?) (map :result))]
        (if-not (empty? failures)
          ^:internal
          {:status :faiure
           :result {:script script
                    :result  results
                    :failures failures}}
          ^:internal
          {:status :success
           :script script
           :result results})))))
