(ns aoc.p10
  (:use [aoc.lib :as lib])
  (:require [clojure.string :as str])
  (:gen-class))

(def filename "p10.in")

(defn read-input-parsed []
  (slurp-lines filename))

(defn contains-char? [charset c]
  (some true? (map #(= c %) charset)))

(defn chunk-start? [c]
  (contains-char? [\( \[ \{ \<] c))

(defn chunk-end? [c]
  (contains-char? [\) \] \} \>] c))

(defn not-illegal? [line]
  (loop [left line
         chunk-depth 0]
    (if-let [c (first left)]
      (if (chunk-start? c)
        (recur (rest left) (inc chunk-depth))
        (if (zero? chunk-depth)
          (recur (rest left) 0)
          (recur (rest left) (dec chunk-depth))))
      (zero? chunk-depth))))

(defn matching-chunk-end [chunk-start]
  (case chunk-start
    \( \)
    \[ \]
    \{ \}
    \< \>))

(defn find-illegal [line]
  (loop [chunks []
         left line]
    (if-let [c (first left)]
           (if (chunk-start? c)
             (recur (cons c chunks) (rest left))
             (if (= c (matching-chunk-end (first chunks)))
               (recur (rest chunks) (rest left))
               c))
           nil)))

(defn score [ending]
  (case ending
    \) 3
    \] 57
    \} 1197
    \> 25137))

(defn find-corrupted [lines]
  (->> (filter #(not (not-illegal? %)) lines)
       (map find-illegal)
       (filter char?)))

(defn score-corrupted [args]
  (->> (find-corrupted args)
       (map score)
       (reduce +)))

(defn -main []
  (let [input (read-input-parsed)]
    (println (score-corrupted input))))
