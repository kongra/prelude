;; Copyright (c) 2017-present Konrad Grzanek
;; Created 2017-04-06
(ns clojure.kongra.prelude.io
  (:require
   [clojure.kongra.ch
    :refer [chSome defchP]])

  (:import
   [java.io
    InputStream ByteArrayInputStream
    File FileInputStream]

   [java.nio.charset
    Charset]))

;; SYSTEM-WIDE CHARACTER ENCODING
(def ^Charset ENCODING (chSome (Charset/forName "UTF-8")))
