(ns ernie.util)

(defn success?
  [any]
  (and (seqable? any)
       (= (first any) :success)))

(defn failure?
  [any]
  (and (seqable? any)
       (or (= (first any) :failure)
           (= (first any) :error))))

(defn remove-nils
  "remove pairs of key-value that has
   nil value from a (possibly nested) map.
   also transform map to nil if all of its
   value are nil"
  [nm]
  (clojure.walk/postwalk
   (fn [el]
     (if (map? el)
       (let [m (into {} (remove (comp nil? second) el))]
         (when (seq m)
           m))
       el))
   nm))

(defn resolve-value
  [env v]
  (if (symbol? v)
    (if (contains? env v)
      (get env v)
      (throw (Exception. (str "Symbol " v " not bound"))))
    v))

(declare bind-params)

(defn bind-params-in-map
  [env m]
  (into (empty m)
    (map
      (fn [[k v]]
        [k (bind-params env v)])
      m)))

(defn bind-params-in-vector
  [env v]
  (into (empty v)
    (map
      (partial bind-params env)
      v)))

(defn bind-params
  [env params]
  (cond
    (map? params)
    (bind-params-in-map env params)
    (vector? params)
    (bind-params-in-vector env params)
    :else
    (resolve-value env params)))
