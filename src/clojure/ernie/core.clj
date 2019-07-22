(ns ernie.core
  (:require
    [ernie.semantics :as semantics])
  (:import
    [ernie.core Action Verify Clean])
  (:gen-class))

(defn cleanup
  [{:keys [evaluated] :as state}]
  (loop [evaluated (keys evaluated)]
    (if (empty? evaluated)
      state
      (let [[[_ target params] & evaluted] evaluated
            clean (get-in state [:methods target :clean])]
        (when clean
          (apply clean [params]))))))

(defn run
  [funcs expressions]
  (let [[status state] (apply semantics/eval|expressions {:methods funcs} expressions)]
    (cleanup state)))
