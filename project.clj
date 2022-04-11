;; Copyright (c) Konrad Grzanek
(defproject kongra/prelude "0.1.23"
  :description  "Predule codebase for Clojure"
  :url          "https://github.com/kongra/prelude"
  :license      {:name   "Eclipse Public License"
                 :url    "http://www.eclipse.org/legal/epl-v10.html"}

  :repositories [["releases" {:url "https://repo.clojars.org" :creds :gpg}]]

  :dependencies [[org.clojure/clojure              "1.11.1"]
                 [primitive-math                    "0.1.6"]
                 [org.clojure/math.numeric-tower    "0.0.5"]
                 [org.apache.commons/commons-lang3 "3.12.0"]
                 [org.clojure/test.check            "1.1.1"]
                 [kongra/ch                        "0.1.30"]]

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
                    :plugins      [[lein-nodisassemble  "0.1.3"]
                                   [cider/cider-nrepl  "0.28.3"]]

                    :middleware   [lein-nodisassemble.plugin/middleware
                                   cider-nrepl.plugin/middleware]

                    :jvm-opts ["-Dclojure.compiler.direct-linking=false"
                               "-XX:+DoEscapeAnalysis"
                               "-Xms1G"
                               "-Xmx1G"]}})
