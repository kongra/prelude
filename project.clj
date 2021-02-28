;; Copyright (c) Konrad Grzanek
;; Created 2019-01-05
(defproject kongra/prelude "0.1.20"
  :description  "Predule codebase for Clojure"
  :url          "https://github.com/kongra/prelude"
  :license      {:name   "Eclipse Public License"
                 :url    "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure                 "1.10.2"]
                 [primitive-math                       "0.1.6"]
                 [org.clojure/math.numeric-tower       "0.0.4"]
                 [org.apache.commons/commons-lang3      "3.11"]
                 #_[org.uncommons.maths/uncommons-maths "1.2.2a"]
                 [org.clojure/test.check               "1.1.0"]
                 [kongra/ch                           "0.1.28"]]

  :aot          :all
  :source-paths ["src/main/clj"]
  :test-paths   ["test/clojure"]

  :java-source-paths ["src/main/java"]

  :global-vars  {*warn-on-reflection* false
                 *print-length*       500}

  ;; :pedantic? :warn

  :jvm-opts ["-Dclojure.compiler.direct-linking=true"]

  :clean-targets ^{:protect false} ["target"]

  :profiles {:repl {:dependencies []
                    :plugins      [[lein-nodisassemble      "0.1.3"]
                                   [cider/cider-nrepl      "0.25.9"]]

                    :middleware   [lein-nodisassemble.plugin/middleware
                                   cider-nrepl.plugin/middleware]

                    :jvm-opts ["-Dclojure.compiler.direct-linking=false"
                               "-XX:+DoEscapeAnalysis"
                               "-Xms1G"
                               "-Xmx1G"
                               ;; "-verbose:gc"
                               ]}})
