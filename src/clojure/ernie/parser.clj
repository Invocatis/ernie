(ns ernie.parser
  (:require
    [clojure.string :as string]
    [clojure.walk :as walk]
    [instaparse.core :as insta]))

(def grammar
  (insta/parser
    "
      root := (block | def | call | INVOKE action | body)*

      def := <'def'> name ASSIGN? expression

      <expression> := value | compound | access | symbol
                    | call | action | metadata-access | method-call
                    | fn | body

      block := name name body
      body := OCB (bind | call | action | block)* expression? CCB

      fn := formals SQUID (body | expression)

      metadata := METADATA map

      metadata-access := METADATA name


      call := expression actuals

      formals := OP (name COMMA)* name? CP

      actuals := ordinal-params | nominal-params

      ordinal-params := OP (expression COMMA)* expression? CP
      nominal-params := OP OP (name-value COMMA)* name-value? CP CP

      bind := name ASSIGN expression

      action := INVOKE name ordinal-params

      name-value := name ASSIGN (expression | same)
                  | symbol

      same := <'%'>

      symbol := name
      name := word

      access := expression ACCESS name

      method-call := expression ACCESS name list

      <compound> := map | list
      value := string | integer | decimal | nothing

      (* Data Types *)
      map := OCB (name-value COMMA)* name-value? CCB
      list := OB (expression COMMA)* expression? CB
      string := SQUOTE squote-string-char* SQUOTE
              | DQUOTE dquote-string-char* DQUOTE
      integer := #'[0-9]+'
      decimal := #'[0-9]*\\.[0-9]+|[0-9]+\\.[0-9]*'
      nothing := #'(?i)nothing'

      (* Simples *)
      <character> := #'[a-zA-Z]'
      word := #'[^{}\\[\\]()\"\\'!:#\\s,.0-9]'#'[^{}\\[\\]()\"\\'!:#\\s.,]*'
      <word-char> := character | #'[+\\-*/]'
      <squote-string-char> := #'[^\\']'
      <dquote-string-char> := #'[^\"]'

      (* Control Symbols *)
      <ASSIGN> := <':'>

      <INVOKE> := <'!'>

      <OP> := <'('>
      <CP> := <')'>

      <OB> := <'['>
      <CB> := <']'>

      <OCB> := <'{'>
      <CCB> := <'}'>

      <PERIOD> := '.'
      <COMMA> := <','>

      <DQUOTE> := <'\"'>
      <SQUOTE> := <'\\''>

      <ACCESS> := <'.'>

      <METADATA> := <'^'>

      <SQUID> := <'=>'>
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
   :ordinal-params #(into [:list] %&)
   :nominal-params #(into [:map] %&)
   :name-value-params vector
   :name-value name-value
   :map #(into [:map] %&)
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
  (with-meta any (merge (meta any) {:source
                                    (string/trim (apply subs source (insta/span any)))})))

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
