;; Copyright (c) 2016-present Konrad Grzanek
;; Created     2014-03-21
;; Re-designed 2016-10-13

(ns kongra.prelude.doclean
  (:refer-clojure :exclude [ensure])
  (:require [kongra.ch :refer :all]))

;; CLEANUP CONTEXT

(defchC chDoclean kongra.prelude.Doclean)

(defn ^kongra.prelude.Doclean create [ ] (kongra.prelude.Doclean.))

(defn close! [d] (chUnit (.close ^kongra.prelude.Doclean d)))

(def ^:dynamic *doclean* nil)

(defmacro with
  [d & body]
  `(binding [*doclean* (chDoclean ~d)] ~@body))

(defn ensure
  []
  (chDoclean (if-let [d *doclean*]
               d
               (throw (IllegalStateException. "No *doclean*")))))

(defn register!
  ([^kongra.prelude.Doclean d f]
   (chIfn f)
   (chUnit (.register d (reify java.io.Closeable (close [this] (f))))))

  ([f]
   (chUnit (register! (ensure) (chIfn f)))))

(defmacro do
  "Executes the body of expressions in a new *doclean*. BEWARE LAZINESS."
  [& body]
  `(let [d# (create)]
     (with d#
       (with-open [dc# d#]
         ~@body))))
