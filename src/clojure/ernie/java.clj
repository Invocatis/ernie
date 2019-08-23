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
   :methods [[addClass [Class] void]
             [addClass [String] void]
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
  (into [] (.getMethods class)))

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

(defn add-class!
  [this class]
  (if (.getConstructor class empty-parameters)
    (swap! (.state this) add-class (resolve-class class))
    (throw (Exception. (str class ": Class added must have empty constructor")))))

(defn run-string
  [this str]
  (core/run @(.state this) str))

(defn run-file
  [this path]
  (run-string this (slurp path)))

(def -addClass add-class!)
(def -runScript run-string)
(def -runFile run-file)
