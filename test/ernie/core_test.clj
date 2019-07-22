(ns ernie.core-test
  (:require [clojure.test :refer :all]
            [ernie.core :refer :all]))

(def case1
  [:block [:bind 'sym [:given "target"]]])
