(ns ernie.util
  (:require
    [clojure.string :as string]
    [clojure.data.json :as json]
    [clojure.stacktrace :refer [print-stack-trace]])
  (:import
    [com.fasterxml.jackson.databind ObjectMapper]
    [java.io StringWriter]))

(defn swap-lr!
  [atom f left right]
  (loop []
    (let [orig @atom
          result (f orig)]
      (if (compare-and-set! atom orig (left result orig))
        (right result)
        (recur)))))

(defn stacktrace-string
  [ex]
  (with-out-str (clojure.stacktrace/print-stack-trace ex)))

(defn stacktrace
  [e]
  (if (nil? e)
    ""
    (let [sts (stacktrace-string e)
          lines (take-while (complement
                             #(or (string/includes? % "ernie")
                                  (string/includes? % "clojure")))
                      (string/split-lines sts))]
      (if (empty? lines)
        sts
        (str
          (apply str (interpose \newline lines))
          \newline
          "CAUSED BY:"
          \newline
          (stacktrace (.getCause e))))
      sts)))

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

(defmacro with-ns
  "Evaluates body in another namespace.  ns is either a namespace
  object or a symbol.  This makes it possible to define functions in
  namespaces other than the current one."
  [ns & body]
  `(binding [*ns* (the-ns ~ns)]
     ~@body))

(defmacro capture-out
  [& body]
  `(let [o# *out*
         e# *err*
         out# (new StringWriter)
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
             jerr-str# (String. (.toByteArray err-baos#))
             out-str# (str out#)
             err-str# (str err#)]
         (System/setOut osout#)
         (System/setErr oserr#)
         (binding [*out* o#, *err* e#]
           (when-not (empty? out-str#)
             (println out-str#))
           (when-not (empty? err-str#)
             (printerr err-str#)))
         (when-not (empty? jout-str#)
           (.println System/out jout-str#))
         (when-not jerr-str#
           (.println System/err jerr-str#))
         {:result result#
          :out (str out# jout-str#)
          :err (str err# jerr-str#)}))))

(defn ainto
  [arr coll]
  (doseq [i (range (alength arr))]
    (aset arr i (get coll i)))
  arr)

(def empty-parameters (make-array java.lang.Class 0))

(def empty-parameters (make-array java.lang.Class 0))

(defn base-obj
  [class]
  (.. class
      (getConstructor empty-parameters)
      (newInstance (object-array 0))))

(def mem-base-obj (memoize base-obj))

(defn wrap-method
  ([method]
   (wrap-method method (mem-base-obj (.getDeclaringClass method))))
  ([method obj]
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
