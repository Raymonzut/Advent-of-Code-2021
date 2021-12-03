(ns aoc.p3
  (:require [clojure.string :as str])
  (:gen-class))

(def filename "p3.in")

(defn toDigits [byteString]
  (map #(Character/digit % 2) byteString))

(defn readInputParsed []
  (->> filename
       slurp
       str/split-lines
       (map toDigits)))

(defn asBase10 [bitArray]
  (->> bitArray
       (map str)
       (reduce str)
       (str "2r")
       (read-string)))

(defn verticalRates [args]
  (loop [report args
         gamma [] ;; most common
         epsilon []] ;; least common
    (let [slice (map first report)
          zeros (count (filter #(= 0 %) slice))
          ones  (count (filter #(= 1 %) slice))]
        (if (empty? (first report))
          (* (asBase10 gamma) (asBase10 epsilon))
          (if (> zeros ones)
            (recur (map rest report) (conj gamma 0) (conj epsilon 1))
            (recur (map rest report) (conj gamma 1) (conj epsilon 0)))))))

(defn take-masked [mask, arr]
  (map second (filter #(first %) (map vector mask arr))))

(defn narrow [cmp args]
  (loop [report args
         partialReport args]
    (let [slice (map first partialReport)
          zeros (count (filter #(= 0 %) slice))
          ones  (count (filter #(= 1 %) slice))]
        (if (= 1 (count report))
          (first report)
          (if (cmp zeros ones)
            (recur (take-masked (map #(= 0 (first %)) partialReport) report)
                   (map rest (filter #(= 0 (first %)) partialReport)))
            (recur (take-masked (map #(= 1 (first %)) partialReport) report)
                   (map rest (filter #(= 1 (first %)) partialReport))))))))

(defn compoundRates [args]
  (* (asBase10 (narrow > args)) (asBase10 (narrow <= args))))


(defn -main []
  (let [input (readInputParsed)]
    (println (verticalRates input))
    (println (compoundRates input))))
