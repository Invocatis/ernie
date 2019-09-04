(ns ernie.java
  (:require
    [instaparse.core :as insta]
    [ernie.core :as core]
    [ernie.parser :as parser])
  (:import [ernie.core Action Clean Verify])
  (:gen-class
   :name ernie.core.Ernie
   :prefix "-"
   :state state
   :init init
   :methods [[loadComponents [Class] void]
             [runScript [String] void]
             [runFile [String] void]]
   :constructors {[] []}))

(defn -init
  []
  [[] (atom {})])

(defn resolve-class
  [class]
  (cond
    (string? class)
    (Class/forName class)
    (class? class) class))

(defn all-methods
  [class]
  (into [] (.getDeclaredMethods class)))

(def empty-parameters (make-array java.lang.Class 0))

(defn base-obj
  [class]
  (.. class
      (getConstructor empty-parameters)
      (newInstance (object-array 0))))

(def mem-base-obj (memoize base-obj))

(defn wrap-test-method
  [method]
  (let [obj (mem-base-obj (.getDeclaringClass method))]
    (fn [& params]
      (try
        (.invoke method obj (to-array params))
        (catch java.lang.reflect.InvocationTargetException e
          (throw (.getCause e)))))))

(defn add-class
  [funcs class]
  (loop [methods (all-methods class)
         funcs funcs]
    (if (empty? methods)
      funcs
      (let [[method & methods] methods]
        (condp #(.isAnnotationPresent %2 %1) method
          Action (recur methods (assoc-in funcs [(keyword (.value (.getAnnotation method Action))) :action] (wrap-test-method method)))
          Verify (recur methods (assoc-in funcs [(keyword (.value (.getAnnotation method Verify))) :verify] (wrap-test-method method)))
          Clean  (recur methods (assoc-in funcs [(keyword (.value (.getAnnotation method Clean)))  :clean]  (wrap-test-method method)))
          (recur (rest methods) funcs))))))

(defn- method-name-value-pair
  [type m]
  [(keyword (.. m (getAnnotation type) value)) (wrap-test-method m)])

(defn all-methods
  [class]
  (.getMethods class))

(defn- all-methods-with-annotation
  [methods type]
  (filterv #(.isAnnotationPresent % type) methods))

(defn load-methods
  [methods type]
  (into {} (map (partial method-name-value-pair type)
                (all-methods-with-annotation methods type))))

(defn- -load
  [class]
  (let [methods (all-methods class)]
    {:action (load-methods methods Action)
     :verify (load-methods methods Verify)
     :clean  (load-methods methods Clean)}))

(defn load!
  [this class]
  (swap! (.state this) merge (-load class)))

(defn run-string
  [this str]
  (core/run @(.state this) str))

(defn run-file
  [this path]
  (run-string this (slurp path)))

(def -runScript run-string)
(def -runFile run-file)
(def -loadComponents load!)
