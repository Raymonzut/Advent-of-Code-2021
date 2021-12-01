(ns aoc.p1
  (:require [clojure.string :as str])
  (:gen-class))

(defn countIncrements [numbers]
  (count (filter #(< (first %) (second %))
                 (drop 1 (map vector (conj numbers 0) numbers)))))

(defn threeWindows [numbers]
  (let [windows (take-while #(= 3 (count %))
                            (map-indexed (fn [i nums] (take 3 (drop i nums)))
                                         (repeat numbers)))]
    (map (partial reduce +) windows)))

(defn -main []
  (let [numbers (map #(Integer/parseUnsignedInt %) (str/split-lines (slurp "p1.in")))]
    (println (countIncrements numbers))
    (println (countIncrements (threeWindows numbers)))))
