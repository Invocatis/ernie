(ns ernie.semantics
  (:require
    [ernie.util :refer :all]
    [clojure.string :as string]
    [clojure.java.io :as io]
    [clojure.walk :as walk]
    [clojure.pprint :refer [pprint]]))

(def ^:dynamic executed)

(declare eval|exp)

(defn verify
  [namespace environment target actuals result]
  (try
    (if-let [verify-fn (get-in namespace [:verify target])]
      (let [result (apply verify-fn result actuals)]
        {:status :success
         :result result})
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

(defn eval|action
  [namespace environment stack [_ target params :as exp]]
  (if-let [action (get-in namespace [:action target])]
    (let [ev-params (eval|exp namespace environment stack params)]
      (if (success? ev-params)
        (let [inv-action (invoke-action namespace environment stack action (:result ev-params) exp)
              details {:name action :args params}]
          (if (success? inv-action)
            (let [verification (verify namespace environment target (:result ev-params) (:result inv-action))]
              (swap! executed conj {:result (:result inv-action)
                                    :target target
                                    :params params})
              (if (success? verification)
                (assoc inv-action :verification verification :details details)
                {:status :failure
                 :verification verification
                 :details details}))
            (assoc inv-action :details details)))
        ev-params))
    {:status :failure
     :result {:expression exp
              :error [:undefined :action target]
              :stack stack}}))

(defn eval|expect
  [namespace environment stack [_ exp expected]]
  (let [ev (eval|exp namespace environment stack exp)
        expected (or expected :success)]
    (if (= :status expected)
      (assoc ev :status :success)
      ev)))

(defn eval|bind
  [namespace environment stack [_ sym exp]]
  (let [ev (eval|exp namespace environment stack exp)]
    (if (success? ev)
      {:status :success
       :environment (assoc environment sym (:result ev))}
      ev)))

(defn actuals->map
  [formals actuals]
  (cond
    (vector? actuals) (zipmap formals actuals)
    (map? actuals) actuals))

(defn eval|list*
  [namespace environment stack vals]
  (if (empty? vals)
    {:status :success
     :result ()
     :namespace namespace
     :environment environment}
    (let [ev (eval|exp namespace environment stack (first vals))]
      (if (success? ev)
        (let [ev-rest (eval|list* namespace (merge environment (:environment ev))
                                  stack (rest vals))]
          (if (success? ev-rest)
            (update ev-rest :result conj (:result ev))
            ev-rest))
        ev))))

(defn eval|run
  [namespace environment stack [_ body]]
  (eval|exp namespace environment stack body))

(defn eval|call
  [namespace environment stack [_ name actuals expected :as exp]]
  (let [ev (eval|exp namespace environment stack actuals)]
    (if (failure? ev)
      ev
      (if-let [[_ name formals body] (get-in namespace [:cases (keyword name) (count (:result ev))])]
        (eval|exp namespace (actuals->map formals (:result ev)) stack body)
        {:status :error
         :details {:name name :args actuals}
         :result {:expression exp
                  :error [:undefined :case name]
                  :stack stack}}))))

(defn eval|body
  [namespace environment stack [_ & body]]
  (let [body (eval|list* namespace environment stack body)]
    (if (success? body)
      (-> body
        (update :result last)
        (assoc :environment environment))
      body)))

(defn eval|scope
  [namespace environment stack [_ body]]
  (with-bindings {#'executed (atom [])}
    (let [result (eval|exp namespace environment stack body)]
      (cleanup namespace @executed)
      result)))

(defn eval|case
  [namespace environment stack [_ name formals & body :as case]]
  {:status :success
   :namespace (update namespace :cases assoc-in [(keyword name) (count formals)] case)})

(defn eval|list
  [namespace environment stack [_ vals]]
  (let [ev (eval|list* namespace environment stack vals)]
    (if (success? ev)
      (update ev :result vec)
      ev)))

(defn eval|map
  [namespace environment stack [_ m]]
  (let [ks (keys m)
        vs (eval|list* namespace environment stack (vals m))]
    (if (success? vs)
      {:status :success
       :result (zipmap ks (:result vs))}
      vs)))

(defn eval|symbol
  [namespace environment stack [_ v :as exp]]
  (if (= v "nothing")
    {:status :success
     :result nil}
    (if-let [[_ v] (find environment v)]
      {:status :success
       :result v}
      {:status :error
       :result {:expression exp
                :error [:undefined :symbol v]
                :stack stack}})))

(defn eval|value
  [namespace environment stack [_ v :as exp]]
  {:status :success
   :result v})

(defn eval|access
  [namespace environment stack [_ target entity :as exp]]
  (let [{:keys [result] :as ev-target} (eval|exp namespace environment stack target)]
    (if (failure? ev-target)
      ev-target
      (if-not (map? result)
        {:status :error
         :result {:expression exp
                  :error [:general (str "Cannot access entity of value type " (type result))]
                  :stack stack}}
        {:status :success
         :result (get result entity)}))))

(defn eval|metadata
  [namespace environment stack [_ map]]
  (let [{:keys [result] :as ev-map} (eval|exp namespace environment stack map)]
    (if (success? ev-map)
      {:status :success
       :result result
       :metadata result}
      ev-map)))

(def dispatch-map
  {:run    eval|run
   :case   eval|case
   :body   eval|body
   :scope  eval|scope
   :call   eval|call
   :bind   eval|bind
   :expect eval|expect
   :action eval|action
   :map    eval|map
   :list   eval|list
   :symbol eval|symbol
   :value  eval|value
   :access eval|access
   :metadata eval|metadata})

(defn eval|exp
  [namespace environment stack [op & args :as exp]]
  (if-let [f (get dispatch-map op)]
    (apply f [namespace environment (conj stack exp) exp])
    {:status :error
     :result {:error [:unexpected-operation op]
              :stack stack}}))

(defn eval|expressions*
  [namespace environment stack exps]
  (if (empty? exps)
    ()
    (let [{:keys [namespace environment status result]
           :or {namespace namespace environment environment}
           :as ev} (eval|exp namespace environment stack (first exps))]
      (cleanup namespace @executed)
      (reset! executed [])
      (if (error? ev)
        (list ev)
        (conj (eval|expressions* namespace environment stack (rest exps)) ev)))))

(defn eval|expressions
  [namespace exps]
  (with-bindings {#'executed (atom [])}
    (let [results (into [] (eval|expressions* namespace {} [] exps))]
      results)))
