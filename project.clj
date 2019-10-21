(defproject ernie "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/data.xml "0.0.8"]
                 [instaparse "1.4.10"]
                 [com.fasterxml.jackson.core/jackson-databind "2.10.0"]]

  :main ^:skip-aot ernie.core
  :source-paths ["src/clojure"]
  :java-source-paths ["src/java"]
  :target-path "target/%s"
  :aot [ernie.java]
  :profiles {:uberjar {:aot :all}})
