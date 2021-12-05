(ns aoc.p1
  (:require [clojure.string :as str])
  (:gen-class))

(def filename "p1.in")

(defn read-input-parsed []
  (->> filename
       slurp
       str/split-lines
       (map #(Integer/parseUnsignedInt %))
       ))

(defn count-increments [numbers]
  (->> numbers
       (map vector (conj numbers 0))
       (drop 1)
       (filter #(< (first %) (second %)))
       (count)
       ))

(defn three-windows [numbers]
  (->> numbers
       (partition 3 1)
       (map (partial reduce +))))

(defn -main []
  (let [numbers (read-input-parsed)]
    (println (count-increments numbers))
    (println (count-increments (three-windows numbers)))))
