(ns ernie.semantics
  (:require
    [ernie.util :refer :all]
    [ernie.log :as l]
    [taoensso.timbre :as log]
    [clojure.string :as string]
    [clojure.test :refer [testing is]])
  (:import
    [java.io ByteArrayOutputStream PrintStream StringWriter])
  (:refer-clojure :exclude [time namespace]))

(def ^:dynamic executed)

(def ^:dynamic script)

(def ^:dynamic suite)

(def ^:dynamic namespace)

(def ^:dynamic environment)

(def ^:dynamic components)

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
  (when-let [verify-fn (get-in components [:verify target])]
    (log/debugf "Verify: %s" target)
    (log/debugf "Args: %s" (trunc (str actuals)  trunc-length))
    (apply verify-fn result actuals)))

(defn cleanup
  [components executed]
  (loop [executed executed]
    (if (empty? executed)
      nil
      (let [{:keys [target args result]} (peek executed)
            clean (get-in components [:clean target])]
        (when clean
          (log/debugf "Clean: %s" target)
          (log/debugf "Args: %s" (trunc (str args) trunc-length))
          (try
            (apply clean result args)
            (catch Exception e
              (log/errorf "ERROR: %s\n%s" target (stacktrace e)))))
        (recur (pop executed))))))

(defn add-shutdown-hook
  [hook]
  (.addShutdownHook
    (Runtime/getRuntime)
    (Thread. hook)))

(defn ->executed
  [components ns name]
  (let [ex (atom [])]
    (add-shutdown-hook
      #(when-not (or (instance? clojure.lang.Var$Unbound components)
                     (empty? components))
         (cleanup components @ex)))
    ex))

(defn ->suite
  [name]
  (log/infof "Suite: %s" name)
  (let [ns-name (symbol (str "test." name))]
    (or
      (get @suites ns-name)
      (let [_ (remove-ns ns-name)
            ns (create-ns ns-name)]
        (intern ns '_executed (->executed components "suite" name))
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
      (log/errorf "ERROR: %s\n%s" target (stacktrace e))
      (throw e))))

(defn eval|def
  [stack [name exp]]
  (swap! namespace assoc (keyword name) exp)
  nil)

(defn eval|action
  [stack [target actuals :as exp]]
  (log/debugf "Action: %s" target)
  (log/debugf "Args: %s" (trunc (str actuals) trunc-length))
  (if-let [action (get-in components [:action target])]
    (let [result (invoke-action target action actuals)]
      (verify target actuals result)
      result)
    (throw (Exception. (format "Action %s Undefined: \n%s" target (l/stack-trace stack))))))

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
  [ns env ex stack {name "name"} statements]
  (fn []
    (binding [namespace (atom ns)
              environment (atom env)
              executed (->executed components "scenario" name)]
      (try
        (eval* stack statements)
        (catch java.lang.Throwable e
          (throw e))
        (finally
          (cleanup components @executed)))
      true)))

(defn handle-scenario
  [exp stack metadata statements]
  (let [test-name (symbol (str (get metadata "name" (name (gensym 'scenario)))))
        test-fn (->test-fn @namespace (with-meta @environment metadata) executed stack metadata statements)]
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
        (report! {:type :fail :message (stacktrace e)
                  :line (l/line-source exp)})))
    (report! {:type :end-test-var :var (ns-resolve suite test-name)})))

(defn eval|block
  [stack [[_ type] [_ name][_ & statements] :as exp]]
  (let [metadata {"name" name}]
    (condp = (keyword type)
      :suite    (handle-suite    stack metadata (vec statements))
      :scenario (handle-scenario exp stack metadata (vec statements))
      (binding [executed (->executed components "block" "name")]
        (try
          (eval* stack statements)
          (catch java.lang.Throwable e
            (throw e))
          (finally
            (cleanup components @executed)))))))

(defn eval|call
  [stack [f actuals :as exp]]
  (log/tracef "Call: %s%s" name actuals)
  (if (fn? f)
    (if (map? actuals)
      (let [formals (-> f meta :formals)]
        (apply f (into [] (map #(get actuals %) formals))))
      (apply f actuals))
    (throw (Exception. (format "Type %s cannot be invoked as a function" (type f))))))

(defn eval|body
  [stack [& body]]
  (last body))

(defn ->fn
  [namespace stack formals body]
  (let [environment @environment]
    ^{:arity #{(count formals)}
      :formals formals}
    (fn [& args]
      (binding [environment (atom (merge environment (zipmap formals args)))
                namespace namespace]
        (eval|exp stack body)))))

(defn eval|fn
  [stack [formals body :as case]]
  (let [formals (eval|exp stack formals)]
    (->fn namespace stack formals body)))

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
  (condp = v
    "nothing" nil
    "true" true
    "false" false
    (if-let [[_ v] (or (find @environment v)
                       (find @namespace (keyword v)))]
      v
      (throw (Exception. (format "Symbol %s Undefined \n%s" v (l/stack-trace stack)))))))

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

(defn eval|metadata-access
  [stack [sym]]
  (get (meta @environment) sym))

(def dispatch-map
  {:block           eval|block
   :def             eval|def
   :fn              eval|fn
   :body            eval|body
   :call            eval|call
   :bind            eval|bind
   :action          eval|action
   :map             eval|map
   :pair            eval|pair
   :list            eval|list
   :symbol          eval|symbol
   :value           eval|value
   :access          eval|access
   :method-call     eval|method-call
   :metadata-access eval|metadata-access})

(def atomic? #{:value :fn :block})

(defn eval|if
  [stack [_ _ [_ pred t f :as x]]]
  (if (eval|exp stack pred)
    (eval|exp stack t)
    (when f
      (eval|exp stack f))))

(defn eval|try
  [stack [_ _ [_ t c]]]
  (try
    (eval|exp stack t)
    (catch Exception e (eval|exp stack c))
    (catch java.lang.AssertionError e (eval|exp stack c))))

(def special? {:if eval|if
               :try eval|try})

(defn eval*
  [stack vals]
  (into [] (map (partial eval|exp stack) vals)))

(defn eval|args
  [stack [op & args :as exp]]
  (if (atomic? op)
    args
    (eval* stack args)))

(def indent (atom 0))

(defn eval|exp
  [stack exp]
  (swap! indent inc)
  (let [result
         (let [[op & args :as exp] exp]
           (log/trace "EXP: %s" exp)
           (if-let [f (and (#{:action :call} op) (special? (keyword (second (second (first args))))))]
             (apply f [stack exp])
             (let [result (eval|args stack exp)]
               (if-let [f (get dispatch-map op)]
                 (apply f [(conj stack exp) result])
                 (throw (new Exception (format "Unexpected Operation: %s" op)))))))]
    (swap! indent dec)
    result))

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
  [ns components suites' exps]
  (binding [namespace (atom ns)
            executed (->executed components "sub" (ns-name suite))
            environment (atom {})]
    (let [result (eval|expressions* [] exps)]
      {:namespace @namespace
       :suites @suites
       :result (last result)})))

(defn root
  [ns components suites' exp]
  (bind-when (not (bound? (var suites))) [suites (atom suites')]
    (binding [suite (->suite 'default)
              components components]
      (report! {:type :begin-test-run})
      (report! {:type :begin-test-ns :ns suite})
      (let [result (sub ns components suites' exp)]
        (report! {:type :end-ns-run :ns suite})
        (update-summary! :duration #(- (System/nanoTime) %))
        (report! @@(ns-resolve suite '_summary))
        result))))

(defn eval|expressions
  [ns components suites' exps]
  (if (not (bound? (var suite)))
    (root ns components suites' exps)
    (sub ns components suites' exps)))
