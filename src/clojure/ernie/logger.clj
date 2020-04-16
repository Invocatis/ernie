(ns ernie.logger
  (:require
    [clojure.string :as string]
    [taoensso.timbre :as log])
  (:gen-class
   :name ernie.core.Logger
   :state state
   :init -init
   :prefix "-"
   :methods [^{:static true} [info [String] void]
             ^{:static true} [warn [String] void]
             ^{:static true} [error [String] void]
             ^{:static true} [debug [String] void]]))

(def sensitive (atom #{}))

(defn set-level!
  [level]
  (log/set-level! (keyword level)))

(defn mark-sensitive
  [word]
  (swap! sensitive conj word))

(defn mask-sensitive
  [s]
  (reduce (fn [acc sens] (string/replace acc sens (apply str (repeat (count sens) "*")))) s @sensitive))

(defn -init
  [])

(defn -info
  [msg]
  (log/info (mask-sensitive msg)))

(defn -warn
  [msg]
  (log/warn (mask-sensitive msg)))

(defn -error
  [msg]
  (log/error (mask-sensitive msg)))

(defn -debug
  [msg]
  (log/debug (mask-sensitive msg)))

(defn swap-config!
  [f & args]
  (apply log/swap-config! f args))
