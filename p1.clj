(ns aoc.p1
  (:require [clojure.string :as str])
  (:gen-class))

(def filename "p1.in")

(defn readInputParsed []
  (->> filename
       slurp
       str/split-lines
       (map #(Integer/parseUnsignedInt %))
       ))

(defn countIncrements [numbers]
  (->> numbers
       (map vector (conj numbers 0))
       (drop 1)
       (filter #(< (first %) (second %)))
       (count)
       ))

(defn threeWindows [numbers]
  (->> numbers
       (partition 3 1)
       (map (partial reduce +))))

(defn -main []
  (let [numbers (readInputParsed)]
    (println (countIncrements numbers))
    (println (countIncrements (threeWindows numbers)))))
