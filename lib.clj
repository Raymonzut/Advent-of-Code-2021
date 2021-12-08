(ns aoc.lib
  (:require [clojure.string :as str])
  (:gen-class))

(defn bi-rangi [p1 p2]
  "Range that works both ways, inclusive end"
  (if (< p1 p2)
    (range p1 (inc p2))
    (reverse (range p2 (inc p1)))))

(defn slurp-lines [filename]
  (->> (slurp filename)
       (str/split-lines)))

(defn slurp-lines-with [line-parser filename]
  (->> (slurp-lines filename)
       (map line-parser)))

(defn slurp-single-number-seq [separator filename]
  (->> (slurp-lines filename)
       (first)
       (#(str/split % separator))
       (map #(Integer/parseUnsignedInt %))))

(defn transpose [rows]
  (loop [columns []
         remaining-rows rows]
    (if (empty? (first remaining-rows))
      columns
      (recur (conj columns (map first remaining-rows))
             (map rest remaining-rows)))))
