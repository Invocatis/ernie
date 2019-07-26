(ns user)

(def state (atom 0))

(add-watch state :printer (fn [k r o n] (println o '-> n)))

(def -methods
  {:add {:action (fn [i] (println "ACTION add" i) (swap! state + i))
         :verify (fn [result i] (println "VERIFY add" result i) (= @state result))
         :clean  (fn [result i] (println "CLEAN add" result i) (swap! state - i))}
   :sub {:action (fn [i] (println "ACTION sub" i) (swap! state - i))
         :verify (fn [result i] (println "VERIFY sub" result i) (= @state result))
         :clean  (fn [result i] (println "CLEAN sub" result i) (swap! state + i))}})

(def -methods
  {:add {:action (fn [x y] (+ x y))
         :verify (fn [result x y] (= result (+ x y)))}
   :sub {:action (fn [x y] (- x y))
         :verify (fn [result x y] (= result (- x y)) false)}})

(def script
  "case add(x, y):
     !! add(x, y)

   case sub(x, y):
     !! sub(x, y)

   case test():
     sub[x:10, y:10]

   case test1():
     add[x:1, y:2]
     !! asdf()

   ? test() -> Failure")

(def s {:methods -methods :failures []})
