(ns ernie.report.cli
  (:require
   [clojure.string :as string]
   [ernie.report.util :refer :all]
   [ernie.report.ansi :as ansi]))

(defn rotate
  [xs]
  (if (empty? xs)
    []
    (into [] (apply map vector xs))))

(defn rpad
  [n s]
  (apply str s (take (- n (count s)) (repeat \space))))

(defn str-count
  [s]
  (count (.replaceAll s "\\e\\[[\\d;]*[^\\d;]" "")))

(defn box
  [s & {:keys [corner side edge padding]
        :or {corner \+ edge \- padding 0 side \|}}]
  (let [lines (string/split-lines s)
        width (->> lines (map str-count) (apply max))
        lines (map (partial rpad width) lines)
        vpad (repeat (int (/ padding 2)) (apply str (repeat width \space)))
        hpad (apply str (repeat padding \space))
        side-width (if (seqable? side) (count side) 1)
        edge (str corner (apply str (repeat (+ width (* 2 padding) (* 2 (- side-width 1))) edge)) corner)
        lines (concat vpad lines vpad)]
    (str
      edge
      \newline
      (apply str (interpose \newline (map #(str side hpad % hpad side) lines)))
      \newline
      edge)))

(defn align-columns
  [columns & {:keys [spacing]
              :or {spacing 1}}]
  (let [counts (map #(+ spacing (apply max (map str-count %))) columns)
        padded (map (fn [column count] (map (partial rpad count) column)) columns counts)]
    (if (empty? padded)
      ""
      (apply str (interpose \newline (apply (partial map str) padded))))))

(defn result-text
  [{:keys [type] :as value}]
  (condp = type
    :fail (ansi/style "Failure" :red :bold)
    :error (ansi/style "Error" :red :bold)
    (ansi/style "Pass" :green :bold)))

(defn report-var
  [var value]
  [(-> var meta :name str) (result-text value)])

(defn report-ns
  [ns vars]
  (if (empty? vars)
    ""
    (str
     "SUITE: " (-> ns ns-name str)
     "\n\n"
     (align-columns (rotate (map (partial apply report-var) (sort-by #(-> % meta :name) vars))) :spacing 10))))

(defn report!
  [results]
  (println (box (apply str (interpose "\n\n" (map (partial apply report-ns) results)))
                :padding 4
                :side \|
                :edge \=)))
