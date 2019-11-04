(ns ernie.util
  (:require
    [clojure.string :as string]
    [clojure.data.json :as json]
    [ernie.util :refer :all])
  (:import
    [com.fasterxml.jackson.databind ObjectMapper]
    [java.io StringWriter]))

(defn success?
  [any]
  (= (:status any) :success))

(defn failure?
  [any]
  (or (= (:status any) :failure)
      (= (:status any) :error)))

(defn error?
  [any]
  (= (:status any) :error))

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

(defn get-script-portion
  [script exp]
  (let [{start :instaparse.gll/start-index end :instaparse.gll/end-index} (meta exp)]
    (string/trim (subs script start end))))

(defn object->edn
  [object]
  (json/read-str (.writeValueAsString (ObjectMapper.) object) :key-fn keyword))

(defmacro printerr
  [& args]
  `(binding [*out* *err*]
     (println ~@args)))

(defmacro capture-out
  [& body]
  `(let [out# (new StringWriter)
         err# (new StringWriter)
         osout# System/out
         oserr# System/err
         out-baos# (ByteArrayOutputStream.)
         err-baos# (ByteArrayOutputStream.)
         sout# (PrintStream. out-baos# true "UTF-8")
         serr# (PrintStream. err-baos# true "UTF-8")]
     (System/setOut sout#)
     (System/setErr serr#)
     (binding [*out* out#, *err* err#]
       (let [result# (do ~@body)
             jout-str# (String. (.toByteArray out-baos#))
             jerr-str# (String. (.toByteArray err-baos#))]
         (System/setOut osout#)
         (System/setErr oserr#)
         (println (str out#))
         (printerr (str err#))
         (.println System/out jout-str#)
         (.println System/err jerr-str#)
         {:result result#
          :out (str out# jout-str#)
          :err (str err# jerr-str#)}))))
