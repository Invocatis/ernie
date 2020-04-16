(ns ernie.java
  (:require
    [ernie.core :as core]
    [ernie.util :refer :all])
  (:import
    [ernie.core Action Clean Verify])
  (:gen-class
   :name ernie.core.Ernie
   :prefix "-"
   :state state
   :init init
   :methods [[loadComponents [Class] void]
             [runScript [String] java.lang.Object]
             [runFile [String] java.lang.Object]
             [report [String] void]
             [report [] void]]
   :constructors {[] []}))

(defn -init
  []
  (let [state (atom {:namespace {} :suites {} :components {}})]
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

(defn wrap-verification-method
  [method]
  (fn [& args]
    (let [result (apply (comp object->edn (wrap-method method)) args)]
      (update result :status (comp keyword str)))))

(defn- method-name-value-pair
  [type m]
  (let [annotations (seq (.getAnnotationsByType m type))]
    (->> annotations
      (map (fn [a] [(symbol (.value a)) (wrap-method m)]))
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
  (swap! (.state this) update :components (partial merge-with merge) (-load class)))

(defn run-string
  [this str]
  (let [{:keys [namespace suites result]} (core/run @(.state this) str)]
    (swap! (.state this) (partial merge-with merge) {:namespace namespace :suites suites})
    result))

(defn run-file
  [this path]
  (let [{:keys [namespace suites result]} (core/run @(.state this) (slurp path) path)]
    (swap! (.state this) (partial merge-with merge) {:namespace namespace :suites suites})
    result))

(defn report
  ([this]
   (core/report (:suites @(.state this))))
  ([this path]
   (core/report (:suites @(.state this)) {:report-dir path})))

(def -runScript run-string)
(def -runFile run-file)
(def -loadComponents load!)
(def -report report)
