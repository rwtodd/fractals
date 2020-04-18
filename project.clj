(defproject org.rwtodd/fractals "0.1.0"
  :description "Clojure program to have fun exploring fractals"
  :url "http://github.com/rwtodd/fractals"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :resources ["resources"]
  :main ^:skip-aot org-rwtodd.fractals.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
