(ns ernie.parser
  (:require
    [instaparse.core :as insta]))

(def grammar
  (insta/parser
    "
      root := (case | CALL (expect | call) | comment | INVOKE action)*

      case := <'case'> symbol formals ASSIGN (bind | call)* action? value?

      expect := call (expectation | wait)*

      call := symbol actuals

      formals := OP ((symbol COMMA)* symbol)? CP

      actuals := map | list

      bind := symbol ASSIGN call

      action := INVOKE symbol list

      name-value-params := ((name-value COMMA)* name-value)?
      ordered-params := ((value COMMA)* value)?

      name-value := symbol ASSIGN (value | same)

      same := <'%'>

      <expectation> := RESULT (success | failure)
      wait := <'...'> (integer | decimal) time-unit
      <time-unit> := word

      success := #'(?i)success'
      failure := #'(?i)failure'

      symbol := word

      value := map | list | string | integer | decimal | symbol | random-string

      <comment> := <'#'#'[^\n]'*>

      <comment> := <'#'#'[^\n]'*>

      (* Data Types *)
      map := OCB name-value-params CCB
           | OB name-value-params CB
      list := OB ordered-params CB
            | OP ordered-params CP
      string := QUOTE string-char* QUOTE
      random-string := <'~'> random-string-char* <'~'>
      integer := digit+
      decimal := digit+ PERIOD digit+

      (* Simples *)
      <character> := #'[a-zA-Z-_]'
      word := character (character | digit)*
      <string-char> := #'[^\"]'

      <random-string-char> := #'[^~]'

      <digit> := #'[0-9]'

      (* Control Symbols *)
      <ASSIGN> := <':'>
      <RESULT> := <'->'>

      <INVOKE> := <'!'>
      <CALL> := <'?'>

      <OP> := <'('>
      <CP> := <')'>

      <OB> := <'['>
      <CB> := <']'>

      <OCB> := <'{'>
      <CCB> := <'}'>

      <PERIOD> := '.'

      <COMMA> := <','>

      <QUOTE> := <'\"'>
    "
    :auto-whitespace :standard))

(defn unique-string
  [& [base]]
  (str base (and base "_") (Long/toString (Long. (str (gensym nil) (.getTime (java.util.Date.)))) 36)))

(defn name-value
  [n v]
  (if (= v [:same])
    [n n]
    [n v]))

(def transform-map
  {:root vector
   :formals vector
   :actuals identity
   :ordered-params vector
   :name-value-params #(into {} %&)
   :name-value name-value
   :success (fn [& _] :success)
   :failure (fn [& _] :failure)
   :integer (comp #(Long. %) str)
   :decimal (comp #(Double. %) str)
   :string str
   :random-string (comp unique-string str)
   :symbol keyword
   :word str
   :digit-str str})

(defn has-meta?
  [any]
  (instance? clojure.lang.IObj any))

(defn with-meta-wrapper
  [f]
  (fn [& args]
    (let [result (apply f args)]
      (if (has-meta? result)
        (with-meta result (meta (first args)))
        result))))

(def transform-map-with-meta
  (into {} (map (fn [[k v]] [k (with-meta-wrapper v)]) transform-map)))

(def transform
  (partial
    insta/transform
    transform-map-with-meta))

(defn parse
  [str]
  (let [parsed (grammar str)]
    (if (insta/failure? parsed)
      parsed
      (transform
        (insta/add-line-and-column-info-to-metadata
         str
         parsed)))))
