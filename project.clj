(defproject kongra/prelude "0.1.0-SNAPSHOT"
  :description      "Predule codebase for Clojure"
  :url              "https://github.com/kongra/prelude"
  :license          {:name   "Eclipse Public License"
                     :url    "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies     [[org.clojure/clojure              "1.8.0"]
                     [primitive-math                   "0.1.5"]
                     [org.apache.commons/commons-lang3   "3.4"]

                     ;; FOR DEV. PURPOSES ONLY
                     [criterium           "0.4.4"]]

  ;; FOR DEV. PURPOSES ONLY
  :plugins          [[cider/cider-nrepl  "0.13.0"]
                     [lein-nodisassemble  "0.1.3"]]

  :main          kongra.prelude
  :aot           :all

  :source-paths      ["src/clj"]
  :java-source-paths ["src/java"]

  :global-vars       {*warn-on-reflection* true
                      *print-length*       500}

  :jvm-opts          ["-server"
                      "-d64"
                      "-Dclojure.compiler.direct-linking=true"

                      "-Xshare:off"
                      "-XX:+AggressiveOpts"
                      "-XX:+DoEscapeAnalysis"
                      "-XX:+UseCompressedOops"
                      ;; "-XX:+UseNUMA" ;; to check: numactl --hardware

                      "-Xms1G"
                      "-Xmx1G"

                      "-XX:+UseParallelGC"
                      "-XX:+UseParallelOldGC"
                      "-XX:NewSize=400m"
                      "-XX:MaxNewSize=400m"
                      "-XX:-UseAdaptiveSizePolicy"
                      "-XX:SurvivorRatio=6"

                      "-XX:+PrintGCDetails"
                      "-XX:+PrintGCTimeStamps"])