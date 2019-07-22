(ns ernie.java
  (:require
    [ernie.core :as core]
    [ernie.parser :as parser]))

(def funcs
  (atom {}))

(defn- resolve-class
  [class]
  (cond
    (string? class)
    (Class/forName class)
    (class? class) class))

(defn all-methods
  [class]
  (into [] (.getMethods class)))

(defn wrap-test-method
  [method]
  (fn [params]
    (try
      (.invoke method [params])
      (catch Exception e
        [:failure]))))

(defn add-class
  [funcs class]
  (loop [methods (all-methods class)
         funcs funcs]
    (if (empty? funcs)
      funcs
      (let [[method & methods] methods]
        (condp #(.isAnnotationPresent %1 %2) method
          Action (recur funcs (update-in methods [(wrap-test-method (.target (.getAnnotation method Action)))  :action]))
          Verify (recur funcs (update-in methods [(wrap-test-method (.target (.getAnnotation method Verify)))  :verify]))
          Clean  (recur funcs (update-in methods [(wrap-test-method (.target (.getAnnotation method Clean)))   :clean]))
          (recur (rest methods) funcs))))))

(defn add-class!
  [class]
  (swap! funcs add-class (resolve-class class)))

(defn run-string
  [str]
  (core/run @funcs (paraser/parse str)))

(defn run-file
  [path]
  (run-string (slurp path)))
