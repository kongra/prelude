(defproject kongra/prelude "0.1.0-SNAPSHOT"
  :description      "Predule codebase for Clojure"
  :url              "https://github.com/kongra/prelude"
  :license          {:name   "Eclipse Public License"
                     :url    "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies     [[org.clojure/clojure                  "1.8.0"]
                     [primitive-math                       "0.1.5"]
                     [org.apache.commons/commons-lang3       "3.4"]
                     [org.uncommons.maths/uncommons-maths "1.2.2a"]]

  :profiles {:repl  {:plugins [[lein-nodisassemble "0.1.3"]]}}

  :aot               :all

  :source-paths      ["src/clj" "test"]
  :java-source-paths ["src/java"]
  :test-paths        ["test"    ]

  :global-vars       {*warn-on-reflection* true
                      *assert*             true
                      *print-length*       500} )
