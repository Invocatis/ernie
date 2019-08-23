(ns ernie.preload)

(defn unique-string
  [& [base]]
  (str (gensym (or (str base "_") "**_")) "_"s (.getTime (java.util.Date.))))

(def preloaded-functions
  {(keyword "~asdf") unique-string})
