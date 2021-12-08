(ns aoc.p2
  (:use [aoc.lib :as lib])
  (:require [clojure.string :as str])
  (:gen-class))

(def filename "p2.in")

(defn parse-instruction [instruction-text]
  (->> (str/split instruction-text #" ")
       (#(vector (first %)
                 (Integer/parseUnsignedInt (second %))))))

(defn read-input-parsed []
  (slurp-lines-with parse-instruction filename))

(defn simulate-part-1 [instructions]
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

(defn simulate-part-2 [instructions]
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
  (let [instruction-set (read-input-parsed)]
    (println (simulate-part-1 instruction-set))
    (println (simulate-part-2 instruction-set))))
