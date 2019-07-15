(ns ernie.core
  (:gen-class))

(def classes (atom #{}))

(defn- resolve-class
  [class]
  (cond
    (string? class)
    (Class/forName class)
    (class? class) class))

(defn add-class
  [class]
  (swap! classes conj (resolve-class class)))

(defn all-methods
  [classes]
  (into [] (map #(.getMethods %) classes)))
