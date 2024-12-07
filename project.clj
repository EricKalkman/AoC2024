(defproject aoc2024 "0.1.0-SNAPSHOT"
  :description "Advent of Code 2024"
  :license {:name "GPL-3.0"
            :url "https://choosealicense.com/licenses/gpl-3.0"
            :comment "GNU General Public License v3.0"
            :year 2024
            :key "gpl-3.0"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/math.numeric-tower "0.1.0"]]
  :main ^:skip-aot aoc2024.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
