(defproject ai-retry "1"
  :description "FIXME: write description"

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [helpers "1"]
                 [quil "2.6.0"]]

  :main ai-retry.genetic-algorithm.main

  :target-path "target/%s"

  :profiles {:uberjar {:aot :all}})
