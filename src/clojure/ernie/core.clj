(ns ernie.core
  (:gen-class))

(def environment
  (atom {}))

(defn- resolve-class
  [class]
  (cond
    (string? class)
    (Class/forName class)
    (class? class) class))

(defn all-methods
  [classes]
  (into [] (map #(.getMethods %) classes)))

(defn add-class
  [environment class]
  (let [methods ]))

(defn run-case
  [classes [target params]]
  (when-not (.invoke (verify target) (java.util.HashMap. params))
    (action classes)))

(defn handle-given
  [method-group params])


(defn run
  [{:keys [given action]}])
