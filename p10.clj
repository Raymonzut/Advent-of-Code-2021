(ns aoc.p10
  (:use [aoc.lib :as lib])
  (:require [clojure.string :as str])
  (:gen-class))

;; NOTE Part II only works on the sample
(def filename "p10.sample.in")
(def filename "p10.in")

(defn read-input-parsed []
  (slurp-lines filename))

(defn contains-char? [charset c]
  (some true? (map #(= c %) charset)))

(defn chunk-start? [c]
  (contains-char? [\( \[ \{ \<] c))

(defn chunk-end? [c]
  (contains-char? [\) \] \} \>] c))

(defn corrupted? [line]
  (loop [left line
         chunk-depth 0]
    (if-let [c (first left)]
      (if (chunk-start? c)
        (recur (rest left) (inc chunk-depth))
        (if (zero? chunk-depth)
          (recur (rest left) 0)
          (recur (rest left) (dec chunk-depth))))
      (not (zero? chunk-depth)))))


(defn incomplete? [line]
  (loop [left line
         chunk-depth 0]
    (if-let [c (first left)]
      (if (chunk-start? c)
        (recur (rest left) (inc chunk-depth))
        (recur (rest left) (dec chunk-depth)))
      (not (zero? chunk-depth)))))

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

(defn find-missing-endings [line]
  (loop [chunks []
         left line]
    (if-let [c (first left)]
      (if (chunk-start? c)
        (recur (cons c chunks) (rest left))
        (recur (rest chunks) (rest left)))
      (map matching-chunk-end chunks))))

(defn score-simple [ending]
  (case ending
    \) 3
    \] 57
    \} 1197
    \> 25137))

(defn score-advanced-single [ending]
  (case ending
    \) 1
    \] 2
    \} 3
    \> 4))

(defn score-advanced [endings]
  (loop [score 0
         left endings]
    (if-let [ending (first left)]
      (recur (+ (score-advanced-single ending)
                (* 5 score))
             (rest left))
      score)))

(defn find-corrupted [lines]
  (->> (filter corrupted? lines)
       (map find-illegal)
       (filter char?)))

(defn find-incomplete-endings [lines]
  (->> (filter incomplete? lines)
       (map find-missing-endings)))

(defn score-corrupted [args]
  (->> (find-corrupted args)
       (map score-simple)
       (reduce +)))

(defn middle-of-odd [score-set]
  (let [l (count score-set)
        m (/ (dec l) 2)]
    (first (drop m score-set))))

(defn score-incomplete [args]
  (->> (find-incomplete-endings args)
       (map score-advanced)
       (sort)
       (middle-of-odd)))

(defn -main []
  (let [input (read-input-parsed)]
    (println (score-corrupted input))
    (println (score-incomplete input))))
