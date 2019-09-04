(ns ernie.semantics
  (:require
    [ernie.util :refer :all]
    [clojure.walk :as walk]))

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
        (let [inv-action (invoke-action namespace environment stack action (:result ev-params) exp)]
          (if (success? inv-action)
            (let [verification (verify namespace environment target (:result ev-params) (:result inv-action))]
              (if (success? verification)
                inv-action
                verification))
            inv-action))
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
    (let [{:keys [namespace environment]
           :or {namespace namespace environment environment}
           :as ev} (eval|exp namespace environment stack (first vals))]
      (if (success? ev)
        (let [ev-rest (eval|list* namespace environment stack (rest vals))]
          (if (success? ev-rest)
            (update ev-rest :result conj (:result ev))
            ev-rest))
        ev))))

(defn eval|call
  [namespace environment stack [_ name actuals expected :as exp]]
  (if-let [[_ name formals & body] (get-in namespace [:cases (keyword name)])]
    (let [ev (eval|exp namespace environment stack actuals)]
      (if (failure? ev)
        ev
        (let [ev-list (eval|list* namespace (actuals->map formals (:result ev)) stack body)]
          (if (success? ev-list)
            (-> ev-list
              (update :result last)
              (assoc :environment environment))
            ev-list))))
    {:status :error
     :result {:expression exp
              :error [:undefined :case name]
              :stack stack}}))

(defn eval|case
  [namespace environment stack [_ name formals & body :as case]]
  {:status :success
   :namespace (update namespace :cases assoc (keyword name) case)})

(defn eval|list
  [namespace environment stack [_ vals]]
  (let [ev (eval|list* namespace environment stack vals)]
    (if (success? ev)
      (update ev :result vec)
      ev)))

(defn eval|map
  [namespace environment stack [_ m]]
  (let [ks (eval|list* namespace environment stack (keys m))
        vs (eval|list* namespace environment stack (vals m))]
    (cond
      (and (success? ks) (success? vs))
      {:status :success
       :result (zipmap (second ks) (second vs))}
      (failure? ks)
      ks
      (failure? vs)
      vs)))

(defn eval|value
  [namespace environment stack [_ v :as exp]]
  (if (keyword? v)
    (if-let [[_ v] (find environment v)]
      {:status :success
       :result v}
      {:status :error
       :result {:expression exp
                :error [:undefined :value v]
                :stack stack}})
    {:status :success
     :result v}))

(def dispatch-map
  {:case   eval|case
   :call   eval|call
   :bind   eval|bind
   :expect eval|expect
   :action eval|action
   :map    eval|map
   :list   eval|list
   :value  eval|value})

(defn eval|exp
  [namespace environment stack [op & args :as exp]]
  (if-let [f (get dispatch-map op)]
    (apply f [namespace environment (conj stack exp) exp])
    {:status :error
     :result {:error [:unexpected-operation op]
              :stack stack}}))

(defn cleanup
  [{:keys [executed] :as state}]
  (loop [executed executed]
    (if (empty? executed)
      state
      (let [[target params return] (peek executed)
            clean (get-in state [:methods :clean target])]
        (when clean
          (apply clean return params))
        (recur (pop executed)))))
  (assoc state :executed []))

(defn eval|expressions*
  [namespace environment stack exps]
  (if (empty? exps)
    ()
    (let [{:keys [namespace environment status result]
           :or {namespace namespace environment environment}
           :as ev} (eval|exp namespace environment stack (first exps))]
      (if (error? ev)
        (list ev)
        (conj (eval|expressions* namespace environment stack (rest exps)) ev)))))

(defn eval|expressions
  [namespace exps]
  (into [] (eval|expressions* namespace {} [] exps)))
