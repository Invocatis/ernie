(ns ernie.semantics)

(declare eval|exp)

(defn- success?
  [any]
  (and (seqable? any)
       (= (first any) :success)))

(defn- failure?
  [any]
  (and (seqable? any)
       (or (= (first any) :failure)
           (= (first any) :error))))

(defn- remove-nils
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

(defn verify
  [state target result]
  (let [verify-fn (get-in state [:funcs target :verify])]))

(defn invoke-action
  [state [_ target params :as exp]]
  (try
    (let [action (get-in state [:funcs target :action])
          result (apply action params)
          verification (verify state target result)]
      (if verification
        [:success state result]
        [:failure state verification]))
    (catch Exception e
      [:error (update state :failures conj {:expression exp
                                            :error [:exception e]})])))

(defn invoke-and-cache-action
  [state [_ target params :as exp]]
  (let [[status state result] (invoke-action state exp)]
    [status (assoc-in state [:cache target params] [status result]) result]))

(defn eval|action
  [state [_ force? target params :as exp]]
  (if force?
    (invoke-and-cache-action state exp)
    (if-let [[status result] (get-in state [:cache target params])]
      (if (verify state target result)
        [status state result]
        (invoke-and-cache-action state exp))
      (invoke-and-cache-action state exp))))

(defn eval|expect
  [state [_ exp expected]]
  (let [[status :as result] (eval|exp state exp)]
    (if (= status :error)
      result
      (if (= status (or expected :success))
        result
        [:failure (update state :failures conj {:expression exp
                                                :error [:expected expected (first result)]})]))))

(defn eval|bind
  [state [_ sym exp body]]
  (let [[status state* result :as exp-result] (eval|exp state exp)]
    (if (success? exp-result)
      (eval|exp (assoc-in state [:environment sym] result) body)
      exp-result)))

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
           body body]
      (if (empty? body)
        [:success state]
        (let [[given & body] body
              result (eval|exp state given)]
          (if (failure? result)
            [:failure state result]
            (recur state body)))))))

(defn eval|call
  [state [_ name actuals expected :as exp]]
  (println (meta exp))
  (if-let [case (get-in state [:cases name])]
    (invoke-case state case actuals)
    [:error (update state :failures conj {:expression exp
                                          :error [:undefined name]})]))

(defn eval|case
  [state [_ name formals & body :as case]]
  [:success (update state :cases assoc name case)])

(defn eval|exp
  [state exp]
  (println 'exp! exp)
  (condp = (first exp)
    :case   (eval|case   state exp)
    :call   (eval|call   state exp)
    :bind   (eval|bind   state exp)
    :expect (eval|expect state exp)
    :action (eval|action state exp)))

(defn eval|expressions
  [state exps]
  (loop [state state
         exps exps]
    (if (empty? exps)
      state
      (let [[exp & exps] exps
            result (eval|exp state exp)]
        (if (failure? result)
          [exp state (second result)]
          (recur (second result) exps))))))
