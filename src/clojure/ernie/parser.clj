(ns ernie.parser
  (:require
    [clojure.string :as string]
    [clojure.walk :as walk]
    [instaparse.core :as insta]))

(def grammar
  (insta/parser
    "
      root := ows (root-element ws)* root-element ows

      <root-element> := (block | def | call | action | body | expression)*

      def := <'def'> ws name (ows ASSIGN ows)? ows expression

      <expression> := value | compound | access | symbol
                    | call | action | metadata-access | method-call
                    | fn | body | block | macro

      block := name ws name ows body
      body := OCB ows ((body-element ows)+ expression? |
                       (body-element ows)* expression)
              ows CCB

      <body-element> := bind | call | action | block

      fn := formals ows SQUID ows (body | expression)
          | OP fn CP

      macro := OP formals CP ows SQUID ows (body | expression)
             | OP macro CP

      metadata := METADATA map

      metadata-access := METADATA name

      call := expression ows actuals

      formals := OP ows (name ws)* name? CP

      actuals := ordinal-params | nominal-params

      ordinal-params := OP ows (expression ws)* expression? ows CP
      nominal-params := OP OP ows (name-value ws)* name-value? ows CP CP

      bind := name ows <'='> ows expression

      action := (ws | beginning) INVOKE ows name ows ordinal-params

      name-value := name ows ASSIGN ows (expression | same)
                  | symbol

      same := <'%'>

      symbol := name
      name := word

      access := expression ACCESS name

      method-call := expression ACCESS name list

      <compound> := map | list
      value := string | integer | decimal | nothing

      (* Data Types *)
      map := OCB ows (map-element ows)* map-element? ows CCB
      map-element := name ows ASSIGN ows (same | expression)

      list := OB (expression ows)* expression? CB
      string := SQUOTE squote-string-char* SQUOTE
              | DQUOTE dquote-string-char* DQUOTE
      integer := #'[0-9]+'
      decimal := #'[0-9]*\\.[0-9]+|[0-9]+\\.[0-9]*'
      nothing := #'(?i)nothing'

      (* Simples *)
      <character> := #'[a-zA-Z]'
      word := #'(?!def[{}\\[\\]()\"\\'!:#\\s.,])[^`%{}\\[\\]()\"\\'!:#\\s,.0-9][^{}\\[\\]()\"\\':#\\s.,]*'
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

      <COMMA> := <','>

      <DQUOTE> := <'\"'>
      <SQUOTE> := <'\\''>

      <ACCESS> := <'.'>

      <METADATA> := <'^'>

      <SQUID> := <'=>'>

      <ws> := <#'(\\s|,)+'>
      <ows> := <#'(\\s|,)*'>
      <beginning> := <#'^'>
    "))

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
   :map-element name-value
   :map #(into [:map] %&)
   :list (partial conj [:list])
   :success (fn [& _] [:value :success])
   :failure (fn [& _] [:value :failure])
   :integer (comp #(Long. %) str)
   :decimal (comp #(Double. %) str)
   :string str
   :word str
   :literal (fn [[_ x]] [:value (with-meta x {:type :literal})])
   :noeval (fn [[_ x]] [:value (with-meta x {:type :noeval})])
   :name (fn [x] [:value (symbol x)])
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
