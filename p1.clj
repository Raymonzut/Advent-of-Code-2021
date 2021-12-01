(ns aoc.p1
  (:require [clojure.string :as str])
  (:gen-class))

(defn countIncrements [numbers]
  (->> numbers
       (map vector (conj numbers 0))
       (drop 1)
       (filter #(< (first %) (second %)))
       (count)
       ))

(defn threeWindows [numbers]
  (->> numbers
       repeat
       (map-indexed (fn [i nums] (take 3 (drop i nums))))
       (take-while #(= 3 (count %)))
       (map (partial reduce +))))

(defn -main []
  (let [numbers (map #(Integer/parseUnsignedInt %) (str/split-lines (slurp "p1.in")))]
    (println (countIncrements numbers))
    (println (countIncrements (threeWindows numbers)))))
