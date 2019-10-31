(ns ernie.java
  (:require
    [clojure.walk :refer [stringify-keys]]
    [clojure.pprint :refer [pprint]]
    [instaparse.core :as insta]
    [ernie.core :as core]
    [ernie.parser :as parser]
    [ernie.log :as log]
    [ernie.util :refer :all])
  (:import
    [ernie.core Action Clean Verify])
  (:gen-class
   :name ernie.core.Ernie
   :prefix "-"
   :state state
   :init init
   :methods [[loadComponents [Class] void]
             [runScript [String] java.util.Map]
             [runFile [String] java.util.Map]]
   :constructors {[] []}))

(defn -init
  []
  (let [state (atom {})]
    [[] state]))

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

(def empty-parameters (make-array java.lang.Class 0))

(defn base-obj
  [class]
  (.. class
      (getConstructor empty-parameters)
      (newInstance (object-array 0))))

(def mem-base-obj (memoize base-obj))

(defn ainto
  [arr coll]
  (doseq [i (range (alength arr))]
    (aset arr i (get coll i)))
  arr)

(defn join-method
  [f1 f2]
  (if (nil? f1)
    f2
    (fn [& args]
      (let [nargs (count args)
            nargs1 (-> f1 meta :method .getParameters alength)
            nargs2 (-> f2 meta :method .getParameters alength)]
        (cond
          (= nargs nargs1) (apply f1 args)
          (= nargs nargs2) (apply f2 args)
          (<= nargs1 nargs nargs2) (apply nargs1)
          (<= nargs2 nargs nargs1) (apply nargs2)
          (<= nargs1 nargs2 nargs) (apply nargs2)
          (<= nargs2 nargs1 nargs) (apply nargs1)
          :else (throw (IllegalArgumentException. "Too few arguments passed")))))))

(defn wrap-test-method
  [method]
  (let [obj (mem-base-obj (.getDeclaringClass method))]
    ^{:method method}
    (fn [& args]
      (try
        (if (.isVarArgs method)
          (let [num-params (alength (.getParameters method))
                varargs-type (.getComponentType (.getType (aget (.getParameters method) (dec num-params))))
                [args varargs] (split-at (dec num-params) args)
                varargs-arr (ainto (make-array varargs-type (count varargs)) (vec varargs))]
            (.invoke method obj (to-array (conj (vec args) varargs-arr))))
          (.invoke method obj (to-array (take (alength (.getParameters method)) args))))
        (catch java.lang.reflect.InvocationTargetException e
          (throw (.getCause e)))))))

(defn wrap-verification-method
  [method]
  (fn [& args]
    (let [result (apply (comp object->edn (wrap-test-method method)) args)]
      (update result :status (comp keyword str)))))

(defn- method-name-value-pair
  [type m]
  (let [annotations (seq (.getAnnotationsByType m type))]
    (->> annotations
      (map (fn [a] [(.value a) (wrap-test-method m)]))
      (into {}))))

(defn all-methods
  [class]
  (.getMethods class))

(defn annotation-present
  [type method]
  (or (.isAnnotationPresent method type)
      (and (.isAnnotationPresent type java.lang.annotation.Repeatable)
           (.isAnnotationPresent method (.value (.getAnnotation type java.lang.annotation.Repeatable))))))

(defn- all-methods-with-annotation
  [methods type]
  (filterv (partial annotation-present type) methods))

(defn load-methods
  [methods type]
  (->> (all-methods-with-annotation methods type)
       (map (partial method-name-value-pair type))
       (reduce (partial merge-with join-method))))

(defn- -load
  [class]
  (let [methods (all-methods class)]
    {:action (load-methods methods Action)
     :verify (load-methods methods Verify)
     :clean  (load-methods methods Clean)}))

(defn load!
  [this class]
  (swap! (.state this) (partial merge-with merge) (-load class)))

(defn run-string
  [this str]
  (let [results (core/run @(.state this) str)]
    (when-let [namespace (-> results :result last :namespace)]
      (reset! (.state this) namespace))
    (clojure.walk/postwalk (fn [any] (if (map? any) (dissoc any :namespace) any)) results)))

(defn run-file
  [this path]
  (run-string this (slurp path)))

(def -runScript run-string)
(def -runFile run-file)
(def -loadComponents load!)
