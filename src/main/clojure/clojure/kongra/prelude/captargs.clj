;; Copyright (c) 2016-present Konrad Grzanek
;; Created 2019-03-06
(ns clojure.kongra.prelude.captargs
  (:require
   [cljc.kongra.ch
    :refer [chSome chOptional chIfn]]))

(set! *warn-on-reflection* true)

(def ^:private CAPT-ARGS (atom {}))

(defn captArgs
  [& {:keys [id f args]}]
  (chSome          id)
  (chOptional chIfn f)

  (if (nil? f)
    (@CAPT-ARGS id)

    (do (swap! CAPT-ARGS assoc id args)
        (apply f args))))

(defn callArgs
  [& {:keys [id f]}]
  (chSome id)
  (chIfn   f)
  (apply f (captArgs :id id)))
