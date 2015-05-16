(defproject hive "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-cljsbuild "1.0.6"]
            [com.keminglabs/cljx "0.6.0"]]
  :dependencies [[org.clojure/clojure "1.6.0"] 
                 [org.clojure/math.combinatorics "0.1.1"]
                 [org.clojure/tools.namespace "0.2.10"]
                 [org.clojure/clojurescript "0.0-3178"]
                 [rm-hull/monet "0.2.1"]]
  :cljx {
    :builds [{:source-paths ["src/cljx"]
              :output-path "target/classes"
              :rules :clj}
             {:source-paths ["src/cljx"]
              :output-path "target/classes"
              :rules :cljs}]}
  :source-paths ["src/clj" "target/classes"]
  :cljsbuild {
    :builds [{
      :source-paths ["src/cljs" "target/classes"]
      :compiler {
        :output-dir "resources/public/js"
        :output-to "resources/public/js/main.js"
        :optimization :whitespace
        :pretty-print true}}]}
  :main ^:skip-aot hive.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
