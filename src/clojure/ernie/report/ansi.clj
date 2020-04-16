(ns ernie.report.ansi)

(def ANSI-CODES
  {
   :reset              "[0m"
   :bright             "[1m"
   :blink-slow         "[5m"
   :underline          "[4m"
   :underline-off      "[24m"
   :inverse            "[7m"
   :inverse-off        "[27m"
   :strikethrough      "[9m"
   :strikethrough-off  "[29m"
   :bold               "[1m"

   :default "[39m"
   :white   "[37m"
   :black   "[30m"
   :red     "[31m"
   :green   "[32m"
   :blue    "[34m"
   :yellow  "[33m"
   :magenta "[35m"
   :cyan    "[36m"

   :bg-default "[49m"
   :bg-white   "[47m"
   :bg-black   "[40m"
   :bg-red     "[41m"
   :bg-green   "[42m"
   :bg-blue    "[44m"
   :bg-yellow  "[43m"
   :bg-magenta "[45m"
   :bg-cyan    "[46m"})

(defn ansi
  "Output an ANSI escape code using a style key.
   (ansi :blue)
   (ansi :underline)
  Note, try (style-test-page) to see all available styles.
  If *use-ansi* is bound to false, outputs an empty string instead of an
  ANSI code. You can use this to temporarily or permanently turn off
  ANSI color in some part of your program, while maintaining only 1
  version of your marked-up text.
  "
  [code]
  (if (keyword? code)
    (str \u001b (get ANSI-CODES code (:reset ANSI-CODES)))
    (str \u001b code)))

(defn style
  "Applies ANSI color and style to a text string.
   (style \"foo\" :red)
   (style \"foo\" :red :underline)
   (style \"foo\" :red :bg-blue :underline)
 "
  [s & codes]
  (str (apply str (map ansi codes)) s (ansi :reset)))

(defn clear!
  []
  (println (ansi "[2J")))

(defn clearln!
  []
  (println (ansi "[K")))

(defn move-to!
  [line column]
  (println (ansi (format "[%;%H" line column))))

(defn move-up!
  [n]
  (println (ansi (str "[" n "A"))))

(defn move-down!
  [n]
  (println (ansi (str "[" n "B"))))

(defn move-right!
  [n]
  (println (ansi (str "[" n "C"))))

(defn move-left!
  [n]
  (println (ansi (str "[" n "D"))))

(defn save!
  []
  (println (ansi "[s")))

(defn recall!
  []
  (println (ansi "[u")))
