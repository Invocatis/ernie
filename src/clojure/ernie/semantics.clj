(ns ernie.semantics
  (:require
    [ernie.util :refer :all]
    [ernie.log :as l]
    [taoensso.timbre :as log]
    [clojure.string :as string]
    [clojure.set :as set]
    [clojure.java.io :as io]
    [clojure.walk :as walk]
    [clojure.pprint :refer [pprint]]
    [clojure.test :refer [testing is]]
    [eftest.runner :as ef]
    [shutdown.core :as shutdown])
  (:import
    [java.io ByteArrayOutputStream PrintStream StringWriter])
  (:refer-clojure :exclude [time namespace]))

(def ^:dynamic executed)

(def ^:dynamic script)

(def ^:dynamic suite)

(def ^:dynamic namespace)

(def ^:dynamic environment)

(def ^:dynamic suites)

(declare eval|exp eval*)

(defn report!
  [value]
  (swap! @(ns-resolve suite '_result) conj value))

(defn update-summary!
  [v f & args]
  (swap! @(ns-resolve suite '_summary) update v #(apply f % args)))

(defmacro time-and-value
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     {:time (/ (double (- (. System (nanoTime)) start#)) 1000000.0)
      :value ret#}))

(def trunc-length 1000)

(defn trunc
  [s n]
  (subs s 0 (min (count s) n)))

(defn verify
  [target actuals result]
  (when-let [verify-fn (get-in @namespace [:verify target])]
    (log/infof "Verify: %s" target)
    (log/debugf "Args: %s" (trunc (str actuals)  trunc-length))
    (apply verify-fn result actuals)))

(defn cleanup
  [namespace executed]
  (loop [executed executed]
    (if (empty? executed)
      nil
      (let [{:keys [target args result]} (peek executed)
            clean (get-in namespace [:clean target])]
        (when clean
          (log/infof "Clean: %s" target)
          (log/debugf "Args: %s" (trunc (str args) trunc-length))
          (try
            (apply clean result args)
            (catch Exception e
              (.printStackTrace e))))
        (recur (pop executed))))))

(defn ->executed
  [namespace ns name]
  (let [ex (atom [])]
    (shutdown/remove-hook! (keyword ns (str name)))
    (shutdown/add-hook!
      (keyword ns (str name))
      #(when-not (or (instance? clojure.lang.Var$Unbound namespace)
                     (empty? @namespace))
         (log/info "INTERRUPT! STARTING CLEANUP")
         (cleanup @namespace @ex)))
    ex))

(defn ->suite
  [name]
  (log/infof "Suite: %s" name)
  (let [ns-name (symbol (str "test." name))]
    (or
      (get @suites ns-name)
      (let [_ (remove-ns ns-name)
            ns (create-ns ns-name)]
        (intern ns '_executed (->executed namespace "suite" name))
        (intern ns '_result (atom []))
        (intern ns '_summary (atom {:test 0 :pass 0 :fail 0 :error 0
                                    :type :summary :duration (System/nanoTime)}))
        (with-ns ns
          (clojure.core/refer 'clojure.core :exclude '[namespace])
          (require '[ernie.semantics :refer :all])
          (require '[clojure.test :refer [deftest]]))
        (swap! suites assoc ns-name ns)
        ns))))

(defn invoke-action
  [target action actuals]
  (try
    (let [result (apply action actuals)]
      (swap! executed conj {:result result
                            :target target
                            :args actuals})
      result)
    (catch Exception e
      (swap! executed conj {:result nil
                            :target target
                            :args actuals})
      (throw e))))
      ; (throw (new Exception (format "ActionException %s : %s" (str target) (str actuals)) e)))))

(defn eval|action
  [stack [target actuals :as exp]]
  (log/infof "Action: %s" target)
  (log/debugf "Args: %s" (trunc (str actuals) trunc-length))
  (if-let [action (get-in @namespace [:action target])]
    (let [result (invoke-action target action actuals)]
      (verify target actuals result)
      result)
    (throw (Exception. (format "Action %s Undefined: \n%s" target (l/stack-trace stack))))))

(defn eval|expect
  [stack [exp expected]]
  (if (= expected :success)
    (eval|exp stack exp)
    (try
      (eval|exp stack exp)
      (catch Error e))))

(defn eval|bind
  [stack [sym exp]]
  (swap! environment assoc sym exp)
  nil)

(defn handle-suite
  [stack metadata statements]
  (binding [suite (->suite (symbol (str (get metadata "name" (name (gensym 'suite))))))
            executed (var-get (ns-resolve suite '_executed))]
    (report! {:type :begin-test-run})
    (report! {:type :begin-test-ns :ns suite})
    (eval* stack statements)
    (report! {:type :end-test-ns :ns suite})))

(defn ->test-fn
  [ns env ex stack {:keys [name doc]} statements]
  (fn []
    (binding [namespace (atom ns)
              environment (atom env)
              executed (->executed namespace "scenario" name)]
      (try
        (eval* stack statements)
        (catch java.lang.Throwable e
          (throw e))
        (finally
          (cleanup @namespace @executed)))
      true)))

(defn failure-message
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
          (failure-message (.getCause e))))
      sts)))

(defn handle-scenario
  [exp stack metadata statements]
  (let [test-name (symbol (str (get metadata "name" (name (gensym 'scenario)))))
        test-fn (->test-fn @namespace @environment executed stack metadata statements)]
    (intern suite (with-meta test-name {:test test-fn})
      (fn [] (clojure.test/test-var (resolve test-name))))
    (report! {:type :begin-test-var :var (ns-resolve suite test-name)})
    (update-summary! :test inc)
    (try
      (test-fn)
      (report! {:type :pass :message nil})
      (update-summary! :pass inc)
      (catch java.lang.Throwable e
        (update-summary! :fail inc)
        (log/error (failure-message e))
        (report! {:type :fail :message (failure-message e)
                  :line (l/line-source exp)})))
    (report! {:type :end-test-var :var (ns-resolve suite test-name)})))

(defn eval|block
  [stack [[_ name] metadata [_ & statements] :as exp]]
  (let [metadata (eval|exp stack metadata)]
    (condp = (keyword name)
      :suite    (handle-suite    stack metadata (vec statements))
      :scenario (handle-scenario exp stack metadata (vec statements))
      (binding [executed (->executed namespace "block" "name")]
        (try
          (eval* stack statements)
          (catch java.lang.Throwable e
            (throw e))
          (finally
            (cleanup @namespace @executed)))))))

(defn eval|call
  [stack [name actuals :as exp]]
  (log/tracef "Call: %s%s" name actuals)
  (if-let [f (get-in @namespace [:cases (keyword name)])]
    (if (map? actuals)
      (let [formals (-> f meta :formals)]
        (apply f (into [] (map #(get actuals %) formals))))
      (apply f actuals))
    (throw (Exception. (format "Case %s Undefined" name)))))

(defn eval|body
  [stack [& body]]
  (last body))

(defn ->case
  [stack formals body]
  ^{:arity #{(count formals)}
    :formals formals}
  (fn [& args]
    (binding [environment (atom (zipmap formals args))]
      (eval|exp stack body))))

(defn join-cases
  [name c0 c1]
  (cond
    (nil? c0) c1
    (nil? c1) c0
    :else
    (let [arity0 (:arity (meta c0))
          arity1 (:arity (meta c1))
          arity-union (set/union arity0 arity1)]
      ^{:arity arity-union}
      (fn [& args]
        (let [nargs (count args)]
          (cond
            (contains? arity0 nargs) (apply c0 args)
            (contains? arity1 nargs) (apply c1 args)
            :else
            (throw
              (new Exception
                (format "%s -- Arity mismatch: Got %s %s but expected one of [%s]"
                        name nargs args
                        (apply str (interpose ", " (sort arity-union))))))))))))

(defn eval|case
  [stack [name formals body :as case]]
  (let [name (eval|exp stack name)
        formals (eval|exp stack formals)]
    ; (swap! namespace update :cases
    ;                  update-in [(keyword name)]
    ;                  (partial join-cases name)
    ;                  (->case stack formals body))
    (swap! namespace update :cases assoc-in [(keyword name)] (->case stack formals body))
    nil))

(defn eval|list
  [stack [& vals]]
  (into [] vals))

(defn eval|map
  [stack [& m]]
  (into {} m))

(defn eval|pair
  [stack [x y]]
  [x y])

(defn eval|symbol
  [stack [v :as exp]]
  (if (= v "nothing")
    nil
    (if-let [[_ v] (or (find @environment v)
                       (find (get @namespace :cases) (keyword v)))]
      v
      (do (def s stack) (throw (Exception. (format "Symbol %s Undefined \n%s" v (l/stack-trace stack))))))))

(defn eval|value
  [stack [v :as exp]]
  v)

(defn capt
  [word]
  (apply str (string/capitalize (first word)) (rest word)))

(defn java-get
  [target field]
  (let [f (.getDeclaredField (class target) (name field))
        is-bool (= "boolean" (string/lower-case (.getSimpleName (.getType f))))
        prefix (if is-bool "is" "get")]
    (.invoke (.getMethod (class target)
                         (str prefix (capt (name field)))
                         (into-array java.lang.Class []))
             target
             (object-array 0))))

(defn eval|access
  [stack [target entity :as exp]]
  (when target
    (cond
      (map? target) (get target entity)
      :else (java-get target entity))))

(defn eval|method-call
  [stack [target method-name args]]
  (when-not (nil? target)
    (let [types (into-array java.lang.Class (map class args))
          f (wrap-method (.getMethod (class target) (name method-name) types) target)]
      (apply f args))))

(defn eval|metadata
  [stack [map]]
  (swap! environment with-meta map)
  map)

(defn eval|metadata-access
  [stack [sym]]
  (get (meta @environment) sym))

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
   :method-call     eval|method-call
   :metadata        eval|metadata
   :metadata-access eval|metadata-access})

(def atomic? #{:expect :value :case :block})

(defn eval|if
  [stack [_ _ [_ pred t f :as x]]]
  (if (eval|exp stack pred)
    (eval|exp stack t)
    (when f
      (eval|exp stack f))))

(def special? {:if eval|if})

(defn eval*
  [stack vals]
  (into [] (map (partial eval|exp stack) vals)))

(defn eval|args
  [stack [op & args :as exp]]
  (if (atomic? op)
    args
    (eval* stack args)))

(defn eval|exp
  [stack [op & args :as exp]]
  (log/trace "EXP: %s" exp)
  (if-let [f (and (#{:action :call} op) (special? (keyword (second (first args)))))]
    (apply f [stack exp])
    (let [result (eval|args stack exp)]
      (if-let [f (get dispatch-map op)]
        (apply f [(conj stack exp) result])
        (throw (new Exception (format "Unexpected Operation: %s" op)))))))

(defn eval|expressions*
  [stack exps]
  (if (empty? exps)
    ()
    (try
      (let [result (eval|exp stack (first exps))]
        (cons result (eval|expressions* stack (rest exps))))
      (finally
        (cleanup @namespace @executed)
        (reset! executed [])))))

(defmacro bind-when
  [condition binds & body]
  `(if ~condition
     (binding ~binds
        ~@body)
     (do ~@body)))

(defmacro bind-unbound
  [[b v :as bind] & body]
  `(if (not (bound? (var ~b)))
     (binding ~bind
       ~@body)
     (do ~@body)))

(defn sub
  [ns suites' exps]
  (binding [namespace (atom ns)
            executed (->executed namespace "sub" (ns-name suite))
            environment (atom {})]
    (let [result (eval|expressions* [] exps)]
      {:namespace @namespace
       :suites @suites
       :result (last result)})))

(defn root
  [ns suites' exp]
  (bind-when (not (bound? (var suites))) [suites (atom suites')]
    (binding [suite (->suite 'default)]
      (report! {:type :begin-test-run})
      (report! {:type :begin-test-ns :ns suite})
      (let [result (sub ns suites' exp)]
        (report! {:type :end-ns-run :ns suite})
        (update-summary! :duration #(- (System/nanoTime) %))
        (report! @@(ns-resolve suite '_summary))
        result))))

(defn eval|expressions
  [ns suites' exps]
  (if (not (bound? (var suite)))
    (root ns suites' exps)
    (sub ns suites' exps)))
