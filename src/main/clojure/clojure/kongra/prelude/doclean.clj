;; Copyright (c) 2016-present Konrad Grzanek
;; Created     2014-03-21
;; Re-designed 2016-10-13
(ns clojure.kongra.prelude.doclean
  (:refer-clojure
   :exclude [ensure])

  (:require
   [clojure.kongra.ch
    :refer [defchP chIfn chUnit]))

;; CLEANUP CONTEXT
(defchP chDoclean (instance? jkongra.prelude.Doclean x))

(defn ^jkongra.prelude.Doclean create [ ] (jkongra.prelude.Doclean.))

(defn close! [^jkongra.prelude.Doclean d] (chUnit (.close d)))

(def ^:dynamic *doclean* nil)

(defmacro exec
  "Executes the body of expressions in a new *doclean*. BEWARE LAZINESS."
  [& body]
  `(with-open [d# (create)]
     (binding [*doclean* d#]
       ~@body)))

(defn ensure
  []
  (chDoclean
   (if-let [d *doclean*]
     d
     (throw (IllegalStateException. "No *doclean*")))))

(defn register!
  ([^jkongra.prelude.Doclean d f]
   (chIfn f)
   (chUnit (.register d (reify java.io.Closeable (close [this] (f))))))

  ([f]
   (chIfn f)
   (chUnit (register! (ensure) f))))
