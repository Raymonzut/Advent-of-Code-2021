(ns aoc.p1
  (:require [clojure.string :as str])
  (:gen-class))

(defn -main []
  (let [numbers (map #(Integer/parseUnsignedInt %) (str/split-lines (slurp "p1.in")))]
    (count (filter #(< (first %) (second %))
                   (drop 1 (map vector (conj numbers 0) numbers))))))
