(ns ernie.semantics
  (:require
    [ernie.util :refer :all]
    [clojure.string :as string]
    [clojure.set :as set]
    [clojure.java.io :as io]
    [clojure.walk :as walk]
    [clojure.pprint :refer [pprint]]
    [clojure.test :refer [testing is]]
    [eftest.runner :as ef]
    [com.rpl.defexception :refer [defexception]])
  (:import
    [java.io ByteArrayOutputStream PrintStream StringWriter])
  (:refer-clojure :exclude [time namespace]))

(defexception VerificationException)

(defexception NoActionDefinedExcpetion)

(defexception NoCaseDefinedException)

(defexception UndefinedSymbolException)

(def ^:dynamic executed)

(def ^:dynamic script)

(def ^:dynamic suite)

(def ^:dynamic namespace)

(def ^:dynamic environment)

(def ^:dynamic suites)

(declare eval|exp eval*)

(defn ->suite
  [name]
  (let [ns-name (symbol (str "test." name))]
    (or
      (get @suites ns-name)
      (let [_ (remove-ns ns-name)
            ns (create-ns ns-name)]
        (intern ns 'executed (atom {}))
        (with-ns ns
          (clojure.core/refer 'clojure.core :exclude '[namespace])
          (require '[ernie.semantics :refer :all])
          (require '[clojure.test :refer [deftest]])
          (def _executed (atom []))
          (def _result (atom [])))
        (swap! suites assoc ns-name ns)
        ns))))

(defmacro time-and-value
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     {:time (/ (double (- (. System (nanoTime)) start#)) 1000000.0)
      :value ret#}))

(defn verify
  [target actuals result]
  (when-let [verify-fn (get-in @namespace [:verify target])]
    (apply verify-fn result actuals)))

(defn cleanup
  [namespace executed]
  (loop [executed executed]
    (if (empty? executed)
      nil
      (let [{:keys [target args result]} (peek executed)
            clean (get-in namespace [:clean target])]
        (when clean
          (try
            (apply clean result args)
            (catch Exception e
              (.printStackTrace e))))
        (recur (pop executed))))))

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
      (throw (new Exception (format "ActionException %s : %s" (str target) (str actuals) e))))))

(defn eval|action
  [stack [target actuals :as exp]]
  (if-let [action (get-in @namespace [:action target])]
    (let [result (invoke-action target action actuals)]
      (verify target actuals result)
      result)
    (throw (->NoActionDefinedExcpetion (str target)))))

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
  (binding [suite (->suite (symbol (str (get metadata "name" "NO_SUITE_NAME_DEFINED"))))
            executed (var-get (ns-resolve suite '_executed))]
    (eval* stack statements)))

(defn ->test-fn
  [ns env ex stack {:keys [name doc]} statements]
  (fn []
    (binding [namespace (atom ns)
              environment (atom env)
              executed (atom [])]
      (try
        (testing (str name \tab doc)
          (is
            (eval* stack statements)))
        (finally
          (cleanup @namespace @executed)))
      true)))

(defn handle-scenario
  [stack metadata statements]
  (let [test-name (symbol (str (get metadata "name" "NO_SCENARIO_NAME_DEFINED")))
        test-fn (->test-fn @namespace @environment executed stack metadata statements)]
    (intern suite (with-meta test-name {:test test-fn})
      (fn [] (clojure.test/test-var (resolve test-name))))
    (ef/run-tests
      [(ns-resolve suite test-name)]
      {:report #(swap! (var-get (ns-resolve suite '_result)) conj %)})))

(defn eval|block
  [stack [[_ name] metadata [_ & statements]]]
  (let [metadata (eval|exp stack metadata)]
    (condp = (keyword name)
      :suite    (handle-suite    stack metadata (vec statements))
      :scenario (handle-scenario stack metadata (vec statements))
      (binding [executed (atom [])]
        (try
          (eval* stack statements)
          (finally
            (cleanup @namespace @executed)))))))

(defn eval|call
  [stack [name actuals :as exp]]
  (if-let [f (get-in @namespace [:cases (keyword name)])]
    (if (map? actuals)
      (let [formals (-> f meta :formals)]
        (apply f (into [] (map #(get actuals %) formals))))
      (apply f actuals))
    (throw (->NoCaseDefinedException name))))

(defn eval|body
  [stack [& body]]
  (last body))

(defn ->case
  [stack formals body]
  ^{:arity #{(count formals)}
    :formals formals}
  (fn [& args]
    (binding [environment (atom (merge @environment (zipmap formals args)))]
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
      (throw (->UndefinedSymbolException v)))))

(defn eval|value
  [stack [v :as exp]]
  v)

(defn eval|access
  [stack [target entity :as exp]]
  (when (map? target)
    (get target entity)))

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
   :metadata        eval|metadata
   :metadata-access eval|metadata-access})

(def atomic? #{:expect :value :case :block})

(defn eval*
  [stack vals]
  (into [] (map (partial eval|exp stack) vals)))

(defn eval|exp
  [stack [op & args :as exp]]
  (let [result (if-not (atomic? op)
                 (eval* stack args)
                 args)]
    (if-let [f (get dispatch-map op)]
      (apply f [(conj stack exp) result])
      (throw (new Exception (format "Unexpected Operation: %s" op))))))

(defn eval|expressions*
  [stack exps]
  (if (empty? exps)
    ()
    (try
      (let [result (eval|exp stack (first exps))]
        (eval|expressions* stack (rest exps)))
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

(defn eval|expressions
  [ns suites' exps]
  (bind-when (not (bound? (var suites))) [suites (atom suites')]
    (bind-when (not (bound? (var suite))) [suite (->suite 'default)]
      (binding [namespace (atom ns)
                executed (atom [])
                environment (atom {})]
        (eval|expressions* [] exps)
        {:namespace @namespace
         :suites @suites}))))
