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

(defn get-method-of-annotation
  [classes annotation target]
  (-> (all-methods classes)
    (filter #(.isAnnotationPresent classes annotation))
    (filter #((= target .target %)))
    first))

(defn action
  [classes target]
  (get-method-of-annotation classes ernie.core.Action target))

(defn verify
  [classes target]
  (get-method-of-annotation classes ernie.core.Verify target))

(defn cleaner
  [classes target]
  (get-method-of-annotation classes ernie.core.Clean target))

(defn run-case
  [classes [target params]]
  (when-not (.invoke (verify target) (java.util.HashMap. params))
    (action classes)))

(defn handle-given)

(defn run
  [{:keys [given action]}])
