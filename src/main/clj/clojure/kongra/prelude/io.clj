;; Copyright (c) 2017-present Konrad Grzanek
;; Created 2017-04-06
(ns clojure.kongra.prelude.io
  (:require
   [cljc.kongra.ch
    :refer [chSome]])

  (:import
   [java.nio.charset Charset]))

(set! *warn-on-reflection* true)

;; SYSTEM-WIDE CHARACTER ENCODING
(def ^Charset ENCODING (chSome (Charset/forName "UTF-8")))
