(defproject sicp "0.1.0-SNAPSHOT"
  :description "Exercises from the SICP"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"] [org.clojure/math.numeric-tower "0.0.2"]]
  :main ^:skip-aot sicp.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :jvm-opts ["-Xss515m"])

