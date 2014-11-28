(defproject game2048 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0-alpha3"]
                 [org.clojure/clojurescript "0.0-2371"]
                 [lonocloud/synthread "1.1.0-SNAPSHOT"]
                 [org.clojure/test.check "0.5.9"]]
  :profiles {:dev {:plugins [[com.keminglabs/cljx "0.4.0"]]}}
  :plugins [[lein-cljsbuild "0.3.2"]]
  :hooks [cljx.hooks] 
  :cljx {:builds [{:source-paths ["src"]
                   :output-path "target/classes"
                   :rules :clj}

                  {:source-paths ["src"]
                   :output-path "target/classes"
                   :rules :cljs}]}
  :cljsbuild {:builds [{:source-paths ["target/classes"]
                        :compiler {:output-to "all.js"
                                   :optimizations :whitespace
                                   :pretty-print true
                                   :source-map "all.jsm"}}]})

