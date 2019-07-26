(ns ernie.log
  (:require
    [clojure.string :as string]))

(def status-map
  {:success "SUCCESS"
   :failure "FAIL"})

(defn expected-error
  [[_ expected actual]]
  (str "Expected " (status-map expected) " but got " (status-map actual)))

(defn undefined-error
  [[_ target nme]]
  (println nme)
  (condp = target
    :case
    (str "Undefined Case: " (name nme))
    :action
    (str "Undefined Action: " (name nme))))

(defn exception-error
  [[_ exception]]
  (with-out-str (.printStackTrace exception)))

(defn syntax-error
  [[_ error]]
  (str "Syntax Error:\n"
       (with-out-str (println error))))

(defn verification-error
  [[_ target params]]
  (str "Verification failed for:" target " with arguments " params))

(defn error-line
  [[error-type :as error]]
  (condp = error-type
    :expected     (expected-error error)
    :undefined    (undefined-error error)
    :exception    (exception-error error)
    :syntax       (syntax-error error)
    :verification (verification-error error)))

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
  (let [lines (subvec file-lines (dec start-line) end-line)]
    (->> lines (interpose "\n\t          | ") (apply str))))

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
      (apply str (interpose "\t" trace))
      (let [exp (peek stack)]
        (if (contains? (meta exp) :instaparse.gll/start-line)
          (let [line (str (stack-line file-lines (meta exp)) ":"
                          (expression-line-numbers (meta exp)) "\n")]
            (if (= line (peek trace))
              (recur (pop stack) trace)
              (recur (pop stack) (conj trace line))))
          (recur (pop stack) trace))))))

(defn log-line
  [file-lines {:keys [expression error stack]}]
  (if (contains? (meta expression) :instaparse.gll/end-line)
    (str (error-line-message (meta expression)) "\n"
         "\t" (error-line error) "\n"
         "\tExpression: " (line-source file-lines (meta expression)) "\n"
         "\tStack Trace:\n"
         "\t" (stack-trace file-lines stack) "\n"
         "\n\n")
    (error-line error)))

(defn generate
  [file-contents failures]
  (apply str
    (count failures) " Failures Encountered\n\n"
    (map
      (partial log-line (string/split-lines file-contents))
      failures)))
