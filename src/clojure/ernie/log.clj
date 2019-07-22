(ns ernie.log
  (:require
    [clojure.string :as string]))

(defn expected-error
  [[_ expected actual]]
  (str "Expected " expected "but got" " actual"))

(defn undefined-error
  [[_ name]]
  (str "Case " name " undefined"))

(defn exception-error
  [[_ exception]]
  (with-out-str (.printStackTrace exception)))

(defn error-line
  [[error-type :as error]]
  (condp = error-type
    :expected  (expected-error error)
    :undefined (undefined-error error)
    :exception (exception-error error)))

(defn log-line
  [file-lines {:keys [expression error]}]
  (let [{start-line :instaparse.gll/start-line} (meta expression)]
    (println file-lines start-line)
    (str "Error on line " start-line ":\n"
         "Expression: " (nth file-lines (dec start-line)) "\n"
         (error-line error)
         "\n\n")))

(defn generate-log
  [file-contents failures]
  (apply str
    (map
      (partial log-line (string/split-lines file-contents))
      failures)))
