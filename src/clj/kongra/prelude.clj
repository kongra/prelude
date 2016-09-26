;; Copyright (c) 2016-present Konrad Grzanek
;; Created 2016-09-26

(ns kongra.prelude
  (:require [primitive-math :as p]))

;; SYS/JVM

(defn room ;:- -> nil
  []
  (let [free-memory  (.. Runtime getRuntime freeMemory)
        total-memory (.. Runtime getRuntime totalMemory)
        max-memory   (.. Runtime getRuntime maxMemory)
        used-memory  (- total-memory free-memory)

        scale (fn [arg] (double (/ arg (* 1024 1024))))]

    (printf "Used  memory : %f MB\n" (scale used-memory))
    (printf "Free  memory : %f MB\n" (scale free-memory))
    (printf "Total memory : %f MB\n" (scale total-memory))
    (printf "Max   memory : %f MB\n" (scale max-memory))

    nil))


(defn gc
  ([] ;:- -> nil
   (gc true))

  ([^Boolean room-after?] ;:- Boolean -> nil
   (System/gc)
   (when (.booleanValue room-after?)
     (room)) nil))


(defmacro with-out-systemout ;:- & sexp -> Object|nil
  [& body]
  `(binding [*out* (java.io.PrintWriter. System/out)] ~@body))


;; ABSTRACTION FOR ALL OBJECTS THAT MAY BE CONVERTED TO MILLISECONDS

(defprotocol Msecs
  (msecs [this])) ;:- Msecs this => this -> x|number?


;; ELAPSED TIME IN MILLIS

(deftype ^:private Stopwatch [^long start])

(defn stopwatch ;:- -> Stopwatch
  []
  (Stopwatch. (System/nanoTime)))


(defn elapsed-msecs ^double ;:- Stopwatch -> double
  [^Stopwatch s]
  (let [start (p/double (.start s))
        end   (p/double (System/nanoTime))]
    (p// (p/- end start) 1e6)))


(extend-protocol Msecs
  Stopwatch
  (msecs [this] (elapsed-msecs this)))
