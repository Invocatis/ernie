(ns ernie.semantics
  (:require
    [ernie.util :refer :all]
    [clojure.walk :as walk]))

(declare eval|exp)

(defn verify
  [{:keys [environment] :as state} target params result]

  (try
    (if-let [verify-fn (get-in state [:methods target :verify])]
      (let [result (apply verify-fn result (walk/postwalk-replace environment params))]
        result)
      true)
    (catch Exception e
      e)))

(defn invoke-action
  [{:keys [environment] :as state} action [_ force? target params :as exp]]
  (try
    (let [actuals (walk/postwalk-replace environment params)
          result (apply action actuals)]
      [:success (update state :executed conj [target actuals result]) result])
    (catch IllegalArgumentException e
      [:error (update state :failures conj {:expression exp
                                            :error [:argument params]
                                            :stack (get state :stack)})])

    (catch Exception e
      [:error (update state :failures conj {:expression exp
                                            :error [:exception e]
                                            :stack (get state :stack)})])))

(defn take-action
  [{:keys [environment] :as state} [_ force? target params :as exp]]
  (if-let [action (get-in state [:methods (keyword target) :action])]
    (let [[status state return :as result] (invoke-action state action exp)]
      (if (failure? result)
        result
        (let [verification (verify state target params return)]
          (if (true? verification)
            [:success state return]
            [:failure
              (update state :failures conj {:expression exp
                                            :error [:verification target params]
                                            :stack (get state :stack)})
             verification]))))
    [:failure (update state :failures conj
                      {:expression exp
                       :error [:undefined :action target]
                       :stack (get state :stack)})]))

(defn take-and-cache-action
  [state [_ force? target params :as exp]]
  (let [[status state result] (take-action state exp)]
    [status (assoc-in state [:cache target params] [status result]) result]))

(defn eval|action
  [state [_ force? target params :as exp]]
  (if force?
    (take-and-cache-action state exp)
    (if-let [[status result] (get-in state [:cache target params])]
      (if (verify state target params result)
        [status state result]
        (take-and-cache-action state exp))
      (take-and-cache-action state exp))))

(defn eval|expect
  [state [_ exp expected]]
  (let [[status :as result] (eval|exp state exp)
        expected (or expected :success)]
    (if (= status :error)
      result
      (if (= status expected)
        result
        [:failure (update state :failures conj {:expression exp
                                                :error [:expected expected (first result)]
                                                :stack (get state :stack)})]))))

(defn eval|bind
  [state [_ sym exp]]
  (let [[status state return :as result] (eval|exp state exp)]
    (if (success? result)
      [status (update state :environment assoc sym return) return]
      result)))

(defn actuals->map
  [formals actuals]
  (cond
    (vector? actuals) (zipmap formals actuals)
    (map? actuals) actuals))

(defn invoke-case
  [state [_ _ formals & body] actuals]
  (let [actuals (actuals->map formals actuals)
        state (update state :environment merge actuals)]
    (loop [state state
           body body
           return nil]
      (if (empty? body)
        [:success state return]
        (let [[given & body] body
              [status state return :as result] (eval|exp state given)]
          (if (failure? result)
            result
            (recur state body return)))))))

(defn eval|call
  [{:keys [environment] :as state} [_ name actuals expected :as exp]]
  (if-let [case (get-in state [:cases (keyword name)])]
    (invoke-case state case (walk/postwalk-replace environment actuals))
    [:error (update state :failures conj {:expression exp
                                          :error [:undefined :case name]
                                          :stack (get state :stack)})]))

(defn eval|call-oob
  [{:keys [environment] :as state} [_ name actuals expected :as exp]]
  (if-let [case (get-in state [:cases (keyword name)])]
    (invoke-case state case (walk/postwalk-replace environment actuals))
    [:error (update state :failures conj {:expression exp
                                          :error [:undefined :case name]
                                          :stack (get state :stack)})]))

(defn eval|case
  [state [_ name formals & body :as case]]
  [:success (update state :cases assoc (keyword name) case)])

(defn dispatch-exp
  [state exp]
  (condp = (first exp)
    :case       (eval|case       state exp)
    :call       (eval|call       state exp)
    :call-oob   (eval|call-oob   state exp)
    :bind       (eval|bind       state exp)
    :expect     (eval|expect     state exp)
    :action     (eval|action     state exp)))

(defn eval|exp
  [state exp]
  (let [state (update state :stack conj exp)
        [status state return :as result] (dispatch-exp state exp)]
    [status (update state :stack pop) return]))

(defn cleanup
  [{:keys [executed] :as state}]
  (loop [executed executed]
    (if (empty? executed)
      state
      (let [[target params return] (peek executed)
            clean (get-in state [:methods target :clean])]
        (when clean
          (apply clean return params))
        (recur (pop executed)))))
  (assoc state :executed []))

(defn eval|expressions
  [state exps]
  (loop [state state
         exps exps]
    (if (empty? exps)
      state
      (let [[exp & exps] exps
            [status state return :as result] (eval|exp state exp)]
        (recur (cleanup state) exps)))))
