(ns ernie.parser
  (:require
    [instaparse.core :as insta]))

(def grammar
  (insta/parser
    "
      root := (case | CALL expect | comment)*

      case := <'case'> symbol formals ASSIGN (bind | call)* action?

      expect := call (expectation | wait)*

      call := symbol actuals

      formals := OP ((symbol COMMA)* symbol)? CP

      actuals := params-by-order | params-by-name

      bind := symbol ASSIGN call

      action := INVOKE symbol params-by-order

      <params-by-name> := OB name-value-params CB
      <params-by-order> := OP ordered-params CP

      name-value-params := ((name-value COMMA)* name-value)?
      ordered-params := ((value COMMA)* value)?

      name-value := symbol ASSIGN value

      <expectation> := RESULT (success | failure)
      wait := <'...'> (integer | decimal) time-unit
      <time-unit> := word

      success := #'(?i)success'
      failure := #'(?i)failure'

      symbol := word

      value := map | list | string | integer | decimal | symbol

      <comment> := <'#'#'[^\n]'*>

      (* Data Types *)
      map := OCB name-value-params CCB
      list := OB ordered-params CB
      string := QUOTE string-char* QUOTE
      integer := digit+
      decimal := digit+ PERIOD digit+

      (* Simples *)
      <character> := #'[a-zA-Z-_]'
      word := character (character | digit)*
      <string-char> := #'[^\"]'

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

(def transform-map
  {:root vector
   :formals vector
   :actuals identity
   :ordered-params vector
   :name-value-params #(into {} %&)
   :name-value vector
   :value identity
   :success (fn [& _] :success)
   :failure (fn [& _] :failure)
   :integer (comp #(Long. %) str)
   :decimal (comp #(Double. %) str)
   :string str
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
