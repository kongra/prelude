;; Copyright (c) Konrad Grzanek
;; Created 2019-01-05
(defproject kongra/prelude "0.1.11"
  :description  "Predule codebase for Clojure"
  :url          "https://github.com/kongra/prelude"
  :license      {:name   "Eclipse Public License"
                 :url    "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure                 "1.10.0"]
                 [primitive-math                       "0.1.6"]
                 [org.clojure/math.numeric-tower       "0.0.4"]
                 [org.apache.commons/commons-lang3       "3.7"]
                 [org.uncommons.maths/uncommons-maths "1.2.2a"]
                 [kongra/ch                           "0.1.12"]

                 [org.clojure/clojurescript "1.10.439"
                  :exclusions [com.google.errorprone/error_prone_annotations
                               com.google.code.findbugs/jsr305]]]

  :plugins      [[lein-cljsbuild "1.1.7"]]

  :aot          :all
  :source-paths ["src/main/clojure"]
  :test-paths   ["test/clojure"]

  :java-source-paths ["src/main/java"]

  :global-vars  {*warn-on-reflection* true
                 *assert*             true
                 *print-length*       500}

  :pedantic? :warn

  :jvm-opts     ["-Dclojure.compiler.direct-linking=true"]

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]

  :aliases {"fig:repl" ["trampoline" "run" "-m" "figwheel.main" "-b" "dev" "-r"]}

  :profiles {:repl {:dependencies [[org.clojure/tools.nrepl "0.2.13"]]
                    :plugins      [[lein-nodisassemble      "0.1.3" ]
                                   [cider/cider-nrepl       "0.19.0"
                                    :exclusions [org.clojure/tools.cli
                                                 org.clojure/tools.namespace
                                                 rewrite-clj]]]}

             :dev  {:dependencies  [[com.bhauman/figwheel-main "0.2.0"
                                     :exclusions [commons-codec args4j]]

                                    [com.bhauman/rebel-readline-cljs "0.1.4"
                                     :exclusions [args4j]]]

                    :source-paths   ["src/main/clojure/cljs"]
                    :resource-paths ["target"]}}

  :cljsbuild
  {:builds
   [{:id "min"
     :source-paths ["src/main/clojure/cljs"]
     :compiler {:output-to       "resources/public/js/compiled/prelude.js"
                :main             cljs.kongra.prelude
                :optimizations    :advanced
                :static-fns       true
                :fn-invoke-direct true
                :pretty-print     false
                :elide-asserts    true
                }}]}

  :figwheel { :css-dirs ["resources/public/css"]})
