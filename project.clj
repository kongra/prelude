(defproject kongra/prelude "0.1.10"
  :description      "Predule codebase for Clojure"
  :url              "https://github.com/kongra/prelude"
  :license          {:name   "Eclipse Public License"
                     :url    "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies     [[org.clojure/clojure                  "1.9.0"]
                     [primitive-math                       "0.1.6"]
                     [org.clojure/math.numeric-tower       "0.0.4"]
                     [org.apache.commons/commons-lang3       "3.7"]
                     [org.uncommons.maths/uncommons-maths "1.2.2a"]
                     [kongra/ch                            "0.1.8"]]

  :profiles {:repl  {:dependencies [[org.clojure/tools.nrepl "0.2.13"]]
                     :plugins      [[lein-nodisassemble       "0.1.3"]
                                    [cider/cider-nrepl       "0.17.0"]]}}
  :aot               :all

  :source-paths      ["src/clj" "test"]
  :java-source-paths ["src/java"      ]
  :test-paths        ["test"          ]

  :global-vars       {*warn-on-reflection* true
                      *assert*             true
                      *print-length*       500}

  :pedantic?         :warn

  :jvm-opts          ["-Dclojure.compiler.direct-linking=true"])
