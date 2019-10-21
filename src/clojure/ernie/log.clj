(ns ernie.log
  (:require
    [clojure.string :as string]
    [clojure.pprint :refer [pprint]]))

(def status-map
  {:success "SUCCESS"
   :failure "FAIL"})

(defn expected-error
  [[_ expected actual]]
  (str "Expected " (status-map expected) " but got " (status-map actual)))

(defn undefined-error
  [[_ target nme :as all]]
  (str "Undefined " (string/capitalize (name target)) ": " (name nme)))

(defn filter-stack-trace
  [lines]
  (take-while (fn [line] (not (re-find #"clojure|ernie" line))) lines))

(defn filter-stack-trace
  [lines]
  (take-while (fn [line] (not (re-find #"clojure|ernie" line))) lines))

(defn exception-error
  [[_ exception]]
  (with-open [sw (java.io.StringWriter.)
              pw (java.io.PrintWriter. sw)]
    (.printStackTrace exception pw)
    (let [stack-trace (clojure.string/split-lines (.toString sw))]
      (apply str "Exception: " (first stack-trace) "\n\t"
        (interpose "\n\t" (filter-stack-trace (rest stack-trace)))))))

(defn syntax-error
  [[_ error]]
  (str "Syntax Error:\n"
       (with-out-str (println error))))

(defn verification-error
  [[_ target params]]
  (str "Verification failed for:" target " with arguments " params))

(defn- unwrap-list
  [l]
  (->> l second (map second)))

(defn argument-error
  [[_ type target params actuals :as error]]
  (condp = type
    :action
    (str "Action argument mismatch for " (name target) params " given " actuals)
    :case
    (str "Case argument mismatch for " (name target) params " given " actuals)))

(defn error-line
  [[error-type :as error]]
  (condp = error-type
    :expected     (expected-error error)
    :undefined    (undefined-error error)
    :exception    (exception-error error)
    :syntax       (syntax-error error)
    :verification (verification-error error)
    :argument     (argument-error error)
    (str error)))

(defn error-line-message
  [{start-line :instaparse.gll/start-line end-line :instaparse.gll/end-line
    start-column :instaparse.gll/start-column end-column :instaparse.gll/end-column}]
  (if (= start-line end-line)
    (str "Error on line [" start-line "]")
    (str "Error on lines[" start-line " - " end-line "]:")))

(defn expression-line-numbers
  [{start-line :instaparse.gll/start-line end-line :instaparse.gll/end-line
    start-column :instaparse.gll/start-column end-column :instaparse.gll/end-column}]
  (if (= start-line end-line)
    (str start-line)
    (str start-line "-" end-line)))

(defn line-source
  [file-lines
   {start-line :instaparse.gll/start-line end-line :instaparse.gll/end-line
    start-column :instaparse.gll/start-column end-column :instaparse.gll/end-column}]
  (let [lines (subvec file-lines start-line end-line)]
    (->> lines (interpose "\n\t            ") (apply str))))

(defn stack-line
  [file-lines
   {start-line :instaparse.gll/start-line end-line :instaparse.gll/end-line
    start-column :instaparse.gll/start-column end-column :instaparse.gll/end-column}]
  (nth file-lines (dec start-line)))

(defn stack-trace
  [file-lines stack]
  (loop [stack stack
         trace []]
    (if (empty? stack)
      (apply str (interpose "\t\t" trace))
      (let [exp (peek stack)]
        (if (contains? (meta exp) :instaparse.gll/start-line)
          (let [line (str "\t" (stack-line file-lines (meta exp)) ": "
                          (expression-line-numbers (meta exp)) "\n")]
            (if (= line (peek trace))
              (recur (pop stack) trace)
              (recur (pop stack) (conj trace line))))
          (recur (pop stack) trace))))))

(defn log-line
  [file-lines {:keys [expression error stack]}]
  (if (contains? (meta expression) :instaparse.gll/end-line)
    (str (error-line-message (meta expression)) "\n"
         "\t" (error-line error) "\n\n"
         "\tExpression: " (line-source file-lines (meta expression)) "\n\n"
         "\tStack Trace:\n"
         "\t" (stack-trace file-lines stack))
    (error-line error)))

(defn generate
  [file-contents failures]
  (apply str
    (count failures) " Failures Encountered\n\n"
    (map
      (partial log-line (string/split-lines file-contents))
      failures)))
