(defproject oc-freddy "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [clj-http-lite "0.3.0"]
                 [cheshire "5.5.0"]
                 [myguidingstar/clansi "1.3.0"]
                 [slingshot "0.10.3"]
                 [aysylu/loom "0.5.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.clojure/core.match "0.2.1"]]
  :main ^:skip-aot oc-freddy.client
  :jvm-opts ["-Xmx2g" "-server"]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
