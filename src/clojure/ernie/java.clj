(ns ernie.java
  (:require
    [instaparse.core :as insta]
    [ernie.core :as core]
    [ernie.parser :as parser])
  (:import [ernie.core Action Clean Verify])
  (:gen-class
   :name ernie.core.Core
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

(defn wrap-test-method
  [method]
  (fn [state & params]
    (.invoke method nil (to-array params))))

(defn add-class
  [funcs class]
  (loop [methods (all-methods class)
         funcs funcs]
    (if (empty? methods)
      (do (println funcs) funcs)
      (let [[method & methods] methods]
        (condp #(.isAnnotationPresent %2 %1) method
          Action (recur methods (assoc-in funcs [(symbol (.value (.getAnnotation method Action))) :action] (wrap-test-method method)))
          Verify (recur methods (assoc-in funcs [(symbol (.value (.getAnnotation method Verify))) :verify] (wrap-test-method method)))
          Clean  (recur methods (assoc-in funcs [(symbol (.value (.getAnnotation method Clean)))  :clean]  (wrap-test-method method)))
          (recur (rest methods) funcs))))))

(defn add-class!
  [this class]
  (swap! (.state this) add-class (resolve-class class)))

(defn run-string
  [this str]
  (core/run @(.state this) str))

(defn run-file
  [this path]
  (run-string this (slurp path)))

(def -addClass add-class!)
(def -runScript run-string)
(def -runFile run-file)
