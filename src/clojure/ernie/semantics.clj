(ns ernie.semantics
  (:require
    [ernie.util :refer :all]
    [clojure.string :as string]
    [clojure.set :as set]
    [clojure.java.io :as io]
    [clojure.walk :as walk]
    [clojure.pprint :refer [pprint]])
  (:import
    [java.io ByteArrayOutputStream PrintStream StringWriter]
    [ernie.core Verification])
  (:refer-clojure :exclude [time]))

(def ^:dynamic executed)

(def ^:dynamic script)

(declare eval|exp eval* eval-with-meta*)

(defmacro time-and-value
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     {:time (/ (double (- (. System (nanoTime)) start#)) 1000000.0)
      :value ret#}))

(defn invoke-action
  [namespace environment stack action actuals exp]
  (try
    (let [result (apply action actuals)]
      {:status :success
       :result result
       :executed [action actuals]})
    (catch Exception e
      {:status :error
       :result {:expression exp
                :error [:exception e]
                :stack stack}})))

(defn verify
  [namespace environment target actuals result]
  (try
    (if-let [verify-fn (get-in namespace [:verify target])]
      (let [result (apply verify-fn result actuals)]
        (cond
          (boolean? result)
          (if result
             {:status :success
              :verification true}
             {:status :failure
              :verifications false})
          (isa? result Verification)
          {:status (keyword (string/lower-case (.getStatus result)))
            :verification result}
          (map? result)
          result
          :else
          (throw (Exception. (str "Verification: type of " (class result) " invalid")))))
      {:status :success
       :result :no-verification-defined})
    (catch Exception e
      {:status :failure
       :result {:error [:verification target actuals e]}})))

(defn cleanup
  [namespace executed]
  (loop [executed executed]
    (if (empty? executed)
      nil
      (let [{:keys [target params result]} (peek executed)
            clean (get-in namespace [:clean target])]
        (when clean
          (try
            (apply clean result params)
            (catch Exception e
              (.printStackTrace e))))
        (recur (pop executed))))))

(defn eval|action-
  [namespace environment stack [target params :as exp]]
  (if-let [action (get-in namespace [:action target])]
    (let [inv-action (invoke-action namespace environment stack action params exp)
          details {:name action :args params}]
      (if (success? inv-action)
        (let [verification (verify namespace environment
                                   target params (:result inv-action))]
          (swap! executed conj {:result (:result inv-action)
                                :target target
                                :params params})
          (if (success? verification)
            (assoc inv-action :verification verification :details details)
            {:status :failure
             :verification verification
             :details details}))
        (assoc inv-action :details details)))
    {:status :failure
     :result {:expression exp
              :error [:undefined :action target]
              :stack stack}}))

(defn eval|action
  [namespace environment stack [target params :as exp]]
  (let [{:keys [out err result]} (capture-out (eval|action- namespace environment stack exp))]
    (assoc result :out out :err err)))

(defn eval|expect
  [namespace environment stack [{:keys [status] :as exp} expected]]
  (if-not (= status expected)
    {:status :failure
     :result {:expression exp
              :error [:expected expected status]
              :stack stack}}
    exp))

(defn eval|bind
  [namespace environment stack [sym exp]]
  {:status :success
   :environment (assoc environment sym exp)})

(defn eval|block
  [namespace environment stack [[_ name] metadata [_ & statements]]]
  (let [ev-meta (eval|exp namespace environment stack metadata)]
    (if (failure? ev-meta)
      ev-meta
      (let [{metadata :result :keys [environment]} ev-meta]
        (let [{:keys [time value]}
              (time-and-value (eval-with-meta* namespace environment stack statements))]
          (assoc value
            :metadata metadata
            :time time
            :type (keyword name)))))))


(defn eval|call
  [namespace environment stack [name actuals :as exp]]
  (if-let [f (get-in namespace [:cases (keyword name)])]
    (apply f [actuals])
    {:status :error
     :details {:name name :args actuals}
     :result {:expression exp
              :error [:undefined :case name]
              :stack stack}}))

(defn eval|body
  [namespace environment stack [& body]]
  {:status :success
   :result (last body)})

(defn actuals->map
  [formals actuals]
  (cond
    (map? actuals) actuals
    :else (zipmap formals actuals)))

(defn ->case
  [namespace environment stack formals body]
  ^{:arity #{(count formals)}}
  (fn [args]
    (let [actuals (actuals->map formals args)
          environment (merge environment actuals)]
      (eval|exp namespace environment stack body))))

(defn join-cases
  [c0 c1]
  (cond
    (nil? c0) c1
    (nil? c1) c0
    :else
    (let [arity0 (:arity (meta c0))
          arity1 (:arity (meta c1))
          arity-union (set/union arity0 arity1)]
      ^{:arity arity-union}
      (fn [args]
        (let [nargs (count args)]
          (cond
            (contains? arity0 nargs) (apply c0 [args])
            (contains? arity1 nargs) (apply c0 [args])
            :else
            (throw
              (new Exception
                (format "Arity mismatch: Got %s but expected one of %s"
                        nargs (apply str (interpose ", " (sort arity-union))))))))))))

(defn eval|case
  [namespace environment stack [name formals body :as case]]
  (let [{name :result} (eval|exp namespace environment stack name)
        {formals :result} (eval|exp namespace environment stack formals)]
    {:status :success
     :namespace (update namespace :cases
                        update-in [(keyword name)]
                        join-cases
                        (->case namespace environment stack formals body))}))

(defn eval|list
  [namespace environment stack [& vals]]
  {:status :success
   :result (into ^:external [] vals)})

(defn eval|map
  [namespace environment stack [& m]]
  {:status :success
   :result (into ^:external {} m)})

(defn eval|pair
  [namespace environment stack [x y]]
  {:status :success
   :result [x y]})

(defn eval|symbol
  [namespace environment stack [v :as exp]]
  (if (= v "nothing")
    {:status :success
     :result nil}
    (if-let [[_ v] (or (find environment v)
                       (find (get namespace :cases) (keyword v)))]
      {:status :success
       :result v}
      {:status :error
       :result {:expression exp
                :error [:undefined :symbol v]
                :stack stack}})))

(defn eval|value
  [namespace environment stack [v :as exp]]
  {:status :success
   :result v})

(defn eval|access
  [namespace environment stack [target entity :as exp]]
  (if-not (map? target)
    {:status :error
     :result {:expression exp
              :error [:general (str "Cannot access entity of value type "
                                    (type target))]
              :stack stack}}
    {:status :success
     :result (get target entity)}))

(defn eval|metadata
  [namespace environment stack [map]]
  {:status :success
   :result map
   :environment (with-meta environment map)
   :metadata map})

(defn eval|metadata-access
  [namespace environment stack [sym]]
  {:status :success
   :result (get (meta environment) sym)})

(def dispatch-map
  {:block           eval|block
   :case            eval|case
   :body            eval|body
   :call            eval|call
   :bind            eval|bind
   :expect          eval|expect
   :action          eval|action
   :map             eval|map
   :pair            eval|pair
   :list            eval|list
   :symbol          eval|symbol
   :value           eval|value
   :access          eval|access
   :metadata        eval|metadata
   :metadata-access eval|metadata-access})

(def atomic? #{:value :case :block})

(def meta? #{:expected})

(defn eval-with-meta*
 [namespace environment stack vals]
 (if (empty? vals)
   {:status :success
    :result ()
    :namespace namespace
    :environment environment}
   (let [ev (eval|exp namespace environment stack (first vals))]
     (if (success? ev)
       (let [ev-rest (eval* namespace (or (:environment ev) environment)
                             stack (rest vals))]
         (if (success? ev-rest)
           (update ev-rest :result conj ev)
           ev-rest))
       ev))))


(defn eval*
  [namespace environment stack vals]
  (if (empty? vals)
    {:status :success
     :result ()
     :namespace namespace
     :environment environment}
    (let [ev (eval|exp namespace environment stack (first vals))]
      (if (success? ev)
        (let [ev-rest (eval* namespace (or (:environment ev) environment)
                              stack (rest vals))]
          (if (success? ev-rest)
            (update ev-rest :result conj (:result ev))
            ev-rest))
        ev))))

(defn -exp
  [namespace environment stack [op & args :as exp]]
  (let [{args :result :as ev-args} (if-not (atomic? op)
                                     (eval* namespace environment stack args)
                                     {:status :success :result args})]
    (if (failure? ev-args)
      ev-args
      (if-let [f (get dispatch-map op)]
        (if (meta? op)
          (apply f [namespace environment (conj stack exp) ev-args])
          (apply f [namespace environment (conj stack exp) args]))
        {:status :error
         :result {:error [:unexpected-operation op]
                  :stack stack}}))))

(defn eval|exp
  [namespace environment stack [op :as exp]]
  (let [{:keys [time value]} (time-and-value (-exp namespace environment stack exp))]
      (assoc value
       :expression exp
       :time time)))

(defn eval|expressions*
  [namespace environment stack exps]
  (if (empty? exps)
    ()
    (let [{:keys [namespace environment status result]
           :or {namespace namespace environment environment}
           :as ev} (eval|exp namespace environment stack (first exps))
           exs @executed]
      (reset! executed [])
      (cleanup namespace exs)
      (if (error? ev)
        (list ev)
        (conj (eval|expressions* namespace environment stack (rest exps))
              (assoc ev :executed exs))))))

(defn eval|expressions
  [namespace exps]
  (binding [executed (atom [])]
    (let [results (into [] (eval|expressions* namespace {} [] exps))]
      results)))
