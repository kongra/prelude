(defproject prelude "0.1.0-SNAPSHOT"
  :description      "Predule codebase for Clojure"
  :url              "https://github.com/kongra/prelude"
  :license {:name   "Eclipse Public License"
            :url    "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.8.0"]
                 ;; FOR DEV. PURPOSES ONLY
                 [criterium "0.4.4"]]

  ;; FOR DEV. PURPOSES ONLY
  :plugins       [[cider/cider-nrepl  "0.13.0"]
                  [lein-nodisassemble "0.1.3" ]]

  :main          kongra.prelude
  :aot           :all

  :source-paths   ["src/clj"]
  :global-vars    {*warn-on-reflection* true
                   *print-length*       500}

  :jvm-opts       ^:replace ["-server"
                             "-Dclojure.compiler.direct-linking=true"])
