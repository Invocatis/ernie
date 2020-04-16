(defproject ernie "0.2.8-SNAPSHOT"
  :description "FIXME: write description"

  :repositories [["atlassian" {:id "atlassian"
                               :url "https://maven.atlassian.com/content/repositories/atlassian-public/"}]]
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/data.xml "0.0.8"]
                 [com.taoensso/timbre "4.10.0"]
                 [com.fzakaria/slf4j-timbre "0.3.16"]
                 [instaparse "1.4.10"]
                 [com.fasterxml.jackson.core/jackson-databind "2.10.0"]
                 [garden "1.3.9"]
                 [io.aviso/pretty "0.1.37"]]
  :main ^:skip-aot ernie.core
  :source-paths ["src/clojure"]
  :java-source-paths ["src/java"]
  :target-path "target/%s"
  :aot [ernie.java ernie.logger]
  :profiles {:uberjar {:aot :all}})
