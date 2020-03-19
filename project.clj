;; Copyright (c) Konrad Grzanek
;; Created 2019-01-05
(defproject kongra/prelude "0.1.19"
  :description  "Predule codebase for Clojure"
  :url          "https://github.com/kongra/prelude"
  :license      {:name   "Eclipse Public License"
                 :url    "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure                 "1.10.0"]
                 [primitive-math                       "0.1.6"]
                 [org.clojure/math.numeric-tower       "0.0.4"]
                 [org.apache.commons/commons-lang3       "3.9"]
                 [org.uncommons.maths/uncommons-maths "1.2.2a"]
                 [kongra/ch                           "0.1.27"]
                 [org.clojure/clojurescript         "1.10.597"]]

  :plugins      [[lein-cljsbuild "1.1.7"]]

  :aot          :all
  :source-paths ["src/main/clojure" "src/main/cljs"]
  :test-paths   ["test/clojure"]

  :java-source-paths ["src/main/java"]

  :global-vars  {*warn-on-reflection* false
                 *print-length*       500}

  ;; :pedantic? :warn

  :jvm-opts ["-Dclojure.compiler.direct-linking=true"]

  :clean-targets ^{:protect false} ["target"]

  :aliases {"fig:repl" ["trampoline" "run" "-m" "figwheel.main" "-b" "dev" "-r"]}

  :profiles {:repl {:dependencies [[org.clojure/test.check  "1.0.0"]]
                    :plugins      [[lein-nodisassemble      "0.1.3"]
                                   [cider/cider-nrepl       "0.25.0-SNAPSHOT"]]

                    :middleware   [lein-nodisassemble.plugin/middleware
                                   cider-nrepl.plugin/middleware]

                    :jvm-opts [;; "-Dclojure.compiler.direct-linking=false"
                               "-XX:+DoEscapeAnalysis"
                               "-XX:+UseCompressedOops"
                               "-Xms1G"
                               "-Xmx1G"
                               ;; "-verbose:gc"
                               ]}

             :dev  {:dependencies  [[com.bhauman/figwheel-main       "0.2.3"]
                                    [com.bhauman/rebel-readline-cljs "0.1.4"]]

                    :source-paths   ["src/main/cljs"]
                    :resource-paths ["target"]}}
  :cljsbuild
  {:builds
   [{:id "min"
     :source-paths ["src/main/cljs"]
     :compiler {:output-to       "resources/public/js/prelude.js"
                :main             cljs.kongra.prelude
                :optimizations    :advanced
                :static-fns       true
                :fn-invoke-direct true
                :pretty-print     false
                :elide-asserts    true
                }}]}

  :figwheel { :css-dirs ["resources/public/css"]})
