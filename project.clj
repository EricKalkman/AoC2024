(defproject aoc2024 "0.1.1-SNAPSHOT"
  :description "Advent of Code 2024"
  :license {:name "GPL-3.0"
            :url "https://choosealicense.com/licenses/gpl-3.0"
            :comment "GNU General Public License v3.0"
            :year 2024
            :key "gpl-3.0"}
  :plugins [[io.taylorwood/lein-native-image "0.3.1"]]
  :native-image {:name "aoc2024"
                 :graal-bin :env/GRAALVM_HOME
                 :opts ["--verbose" "--initialize-at-build-time"]}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/math.numeric-tower "0.1.0"]
                 [org.clojure/data.priority-map "1.2.0"]
                 [structural "0.2.0-SNAPSHOT"]] ; typehinting-macros
  :main ^:skip-aot aoc2024.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true -Xss4m"]}
             :native-image {:jvm-opts ["-Dclojure.compiler.direct-linking=true"]}
             :repl {:jvm-opts ["-Xss4m"]}})
