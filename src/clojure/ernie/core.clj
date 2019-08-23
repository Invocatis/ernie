(ns ernie.core
  (:require
    [instaparse.core :as insta]
    [ernie.parser :refer [parse]]
    [ernie.semantics :as semantics]
    [ernie.log :as log])
  (:import
    [ernie.core Action Verify Clean]))

(defn run
  [funcs script]
  (let [expressions (parse script)]
    (if (insta/failure? expressions)
      (let [failures [{:error [:syntax expressions]}]
            result [:failure {:failures failures}]]
        result)
      (let [state (apply semantics/eval|expressions
                    [{:methods funcs :failures []
                      :executed [] :environment {}
                      :stack []}
                     expressions])]
        (let [failures (get-in state [:failures])]
          (if-not (empty? failures)
            (println (log/generate script failures))
            (println "Success!")))
        state))))
