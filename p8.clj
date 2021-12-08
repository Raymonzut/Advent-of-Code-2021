(ns aoc.p8
  (:use [aoc.lib :as lib])
  (:require [clojure.string :as str])

  (:gen-class))

;; Part II was solved using brute-force,
;; not the real solution, nor something to share

(def filename "p8.in")

(defn parse-line [line]
  (->> (str/split line #" ")
       (map count)))

(defn take-output [text]
  (->> (str/split text #" \| ")
       (drop 1)
       (first)
       (parse-line)))

(defn read-outputs []
  (->> (slurp-lines-with take-output filename)
       (reduce concat)))

;;dig:cnt
;; 1 : 2
;; 4 : 4
;; 7 : 3
;; 8 : 7
(defn count-1478 [args]
  (let [counts (lib/as-count-vec 10 args)]
    (+ (nth counts 2)
       (nth counts 3)
       (nth counts 4)
       (nth counts 7))))

(defn -main []
  (println (count-1478 (read-outputs))))
