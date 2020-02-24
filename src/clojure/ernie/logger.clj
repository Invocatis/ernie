(ns ernie.logger
  (:require
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

(defn -init
  [])

(defn -info
  [msg]
  (log/info msg))

(defn -warn
  [msg]
  (log/warn msg))

(defn -error
  [msg]
  (log/error msg))

(defn -debug
  [msg]
  (log/debug msg))
