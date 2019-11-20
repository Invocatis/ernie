(ns ernie.parser
  (:require
    [clojure.string :as string]
    [clojure.walk :as walk]
    [clojure.java.io :as io]
    [instaparse.core :as insta]))

(def grammar
  (insta/parser
    "
      root := (block | case | CALL (expect | call) | INVOKE action)*

      <expression> := value | access | symbol | compound | call | action | metadata-access

      block := name SQUID metadata body END-SQUID
      run := <'run'> ASSIGN body
      case := <'case'> name formals ASSIGN body
      body := (bind | call | expect | action | block)* expression?

      metadata := METADATA map

      metadata-access := METADATA name

      expect := call expectation

      call := name actuals

      formals := OP ((name COMMA)* name)? CP

      actuals := map | list

      bind := name ASSIGN expression

      action := INVOKE name list

      name-value-params := ((name-value COMMA)* name-value)?

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
      list := OP ((expression COMMA)* expression)? CP
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

      <METADATA> := <'^'>

      <SQUID> := <'=>'>
      <END-SQUID> := <'|>'>
    "
    :auto-whitespace :standard))

(defn name-value
  ([[_ n]] [:pair n [:symbol n]])
  ([n v]
   (if (= v [:same])
     [:pair n [:symbol n]]
     [:pair n v])))


(def transform-map
  {:root vector
   :formals (partial conj [:list])
   :actuals identity
   :ordered-params vector
   :name-value-params vector
   :name-value name-value
   :map (partial into [:map])
   :list (partial conj [:list])
   :success (fn [& _] [:value :success])
   :failure (fn [& _] [:value :failure])
   :integer (comp #(Long. %) str)
   :decimal (comp #(Double. %) str)
   :string str
   :word str
   :name (fn [x] [:value x])
   :nothing (fn [& _] nil)
   :digit-str str})

(def transform
  (partial
    insta/transform
    transform-map))

(defn trim-comments
  [script]
  (->> script
    string/split-lines
    (map #(string/split % #"#"))
    (map first)
    (remove empty?)
    (interpose \newline)
    (apply str)))

(defn sourcable?
  [any]
  (contains? (meta any) :instaparse.gll/start-index))

(defn add-source*
  [source any]
  (with-meta any (merge (meta any) {:source (string/trim (apply subs source (insta/span any)))})))

(defn add-source
  [source result]
  (walk/postwalk
    (fn [any]
      (if (sourcable? any)
        (add-source* source any)
        any))
    result))

(defn parse
  [str]
  (let [source (trim-comments str)
        parsed (grammar source)]
    (if (insta/failure? parsed)
      parsed
      (add-source source (transform parsed)))))
