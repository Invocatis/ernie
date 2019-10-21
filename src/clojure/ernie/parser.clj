(ns ernie.parser
  (:require
    [clojure.string :as string]
    [clojure.java.io :as io]
    [instaparse.core :as insta]))

(def grammar
  (insta/parser
    "
      root := (run | case | CALL (expect | call) | INVOKE action | METADATA metadata)*

      <expression> := value | access | symbol | compound | call | action | metadata

      run := <'run'> ASSIGN body
      case := <'case'> name formals ASSIGN body
      body := (bind | expression | scope)*
      scope := SCOPE body SCOPE

      metadata := METADATA map

      expect := call expectation?

      call := name actuals

      formals := OP ((name COMMA)* name)? CP

      actuals := map | list

      bind := name ASSIGN expression

      action := INVOKE name list

      name-value-params := ((name-value COMMA)* name-value)?
      ordered-params := ((expression COMMA)* expression)?

      name-value := name ASSIGN (expression | same)
                  | symbol

      same := <'%'>

      <expectation> := RESULT (success | failure)
      wait := <'...'> (integer | decimal) time-unit
      <time-unit> := word

      success := #'(?i)success'
      failure := #'(?i)failure'

      symbol := name
      name := word

      access := (map | symbol | access) ACCESS name

      <compound> := map | list
      value := string | integer | decimal | nothing

      (* Data Types *)
      map := OCB name-value-params CCB
           | OB name-value-params CB
      list := OP ordered-params CP
      string := QUOTE string-char* QUOTE
      integer := digit+
      decimal := digit+ PERIOD digit+
      nothing := #'(?i)nothing'

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

      <ACCESS> := <'.'>

      <SCOPE> := <'**'>
      <METADATA> := <'^'>
    "
    :auto-whitespace :standard))

(defn name-value
  ([[_ n]] [n [:symbol n]])
  ([n v]
   (if (= v [:same])
     [n [:symbol n]]
     [n v])))


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
   :word str
   :name str
   :nothing (fn [& _] nil)
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

(defn trim-comments
  [script]
  (->> script
    string/split-lines
    (map #(string/split % #"#"))
    (map first)
    (remove empty?)
    (interpose \newline)
    (apply str)))

(defn parse
  [str]
  (let [parsed (grammar (trim-comments str))]
    (if (insta/failure? parsed)
      parsed
      (transform
        (insta/add-line-and-column-info-to-metadata
         str
         parsed)))))
