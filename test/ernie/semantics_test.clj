(ns ernie.semantics-test
  (:require
    [clojure.test :refer :all]
    [ernie.semantics :refer :all]))
; 
; (deftest bind-params-test
;   (testing "Parameter Binding"
;     (are [env params expected] (= (bind-params env params) expected)
;       '{a 1} '{a a} '{a 1}
;       '{a 1} '[a] [1]
;       '{a 1} '{a [a]} '{a [1]}
;       '{a 1} '{a {a a}} '{a {a 1}})))
