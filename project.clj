(defproject ernie "0.1.1"
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
                 [eftest "0.5.9"]
                 [shutdown "0.1.0-SNAPSHOT"]]
  :main ^:skip-aot ernie.core
  :source-paths ["src/clojure"]
  :java-source-paths ["src/java"]
  :target-path "target/%s"
  :aot [ernie.java]
  :profiles {:uberjar {:aot :all}})
