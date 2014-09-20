(defproject oc-freddy "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0-alpha1"]
                 [clj-http "0.7.8"]
                 [cheshire "5.3.1"]
                 [slingshot "0.10.3"]
                 [org.clojure/core.match "0.2.1"]]
  :main ^:skip-aot oc-freddy.client
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
