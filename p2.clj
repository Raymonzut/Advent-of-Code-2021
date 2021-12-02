(ns aoc.p2
  (:require [clojure.string :as str])
  (:gen-class))

(def filename "p2.in")

(defn readInputParsed []
  (->> filename
       slurp
       str/split-lines
       (map #(str/split % #" "))
       (map #(vector (first %) (Integer/parseUnsignedInt (second %))))))

(defn simulatePart1 [instructions]
  (loop [horizontal 0
         depth 0
         instructions instructions]
    (let [instruction (first instructions)
          amount (second instruction)]
      (case (first instruction)
        "forward" (recur (+ horizontal amount) depth (rest instructions))
        "down" (recur horizontal (+ depth amount) (rest instructions))
        "up" (recur horizontal (- depth amount) (rest instructions))
        (* horizontal depth)))))

(defn simulatePart2 [instructions]
  (loop [horizontal 0
         depth 0
         aim 0
         instructions instructions]
    (let [instruction (first instructions)
          amount (second instruction)]
      (case (first instruction)
        "forward" (recur (+ horizontal amount) (+ depth (* aim amount)) aim (rest instructions))
        "down" (recur horizontal depth (+ aim amount) (rest instructions))
        "up" (recur horizontal depth (- aim amount) (rest instructions))
        (* horizontal depth)))))

(defn -main []
  (let [instructionSet (readInputParsed)]
    (println (simulatePart1 instructionSet))
    (println (simulatePart2 instructionSet))))
