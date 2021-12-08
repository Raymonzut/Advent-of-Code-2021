(ns aoc.p3
  (:use [aoc.lib :as lib])
  (:gen-class))

(def filename "p3.in")

(defn to-digits [byteString]
  (map #(Character/digit % 2) byteString))

(defn read-input-parsed []
  (slurp-lines-with to-digits filename))

(defn as-base10 [bitArray]
  (->> bitArray
       (map str)
       (reduce str)
       (str "2r")
       (read-string)))

(defn vertical-rates [args]
  (loop [report args
         gamma [] ;; most common
         epsilon []] ;; least common
    (let [slice (map first report)
          zeros (count (filter #(= 0 %) slice))
          ones  (count (filter #(= 1 %) slice))]
      (if (empty? (first report))
        (* (as-base10 gamma) (as-base10 epsilon))
        (if (> zeros ones)
          (recur (map rest report) (conj gamma 0) (conj epsilon 1))
          (recur (map rest report) (conj gamma 1) (conj epsilon 0)))))))

(defn take-masked [mask, arr]
  (map second (filter #(first %) (map vector mask arr))))

(defn narrow [cmp args]
  (loop [report args
         partial-report args]
    (let [slice (map first partial-report)
          zeros (count (filter #(= 0 %) slice))
          ones  (count (filter #(= 1 %) slice))]
        (if (= 1 (count report))
          (first report)
          (if (cmp zeros ones)
            (recur (take-masked (map #(= 0 (first %)) partial-report) report)
                   (map rest (filter #(= 0 (first %)) partial-report)))
            (recur (take-masked (map #(= 1 (first %)) partial-report) report)
                   (map rest (filter #(= 1 (first %)) partial-report))))))))

(defn compound-rates [args]
  (* (as-base10 (narrow > args)) (as-base10 (narrow <= args))))


(defn -main []
  (let [input (read-input-parsed)]
    (println (vertical-rates input))
    (println (compound-rates input))))
