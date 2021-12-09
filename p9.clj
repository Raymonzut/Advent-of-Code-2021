(ns aoc.p9
  (:use [aoc.lib :as lib])
  (:require [clojure.string :as str])
  (:gen-class))

(def filename "p9.in")

(defn to-digits [byteString]
  (map #(Character/digit % 10) byteString))

(defn read-input-parsed []
  (slurp-lines-with to-digits filename))

(def wall 10)
(defn surround-with-highs [height-map]
  (->> height-map
       (map #(conj (into [] %) wall))
       (map #(cons wall %))
       (#(cons (map (fn [_] wall) (first %)) %))
       (#(reverse (conj (reverse %) (first %))))))

(defn neighbours [height-map x y]
  (vector
    (nth (nth height-map y) (dec x))
    (nth (nth height-map (dec y)) x)
    (nth (nth height-map y) (inc x))
    (nth (nth height-map (inc y)) x)))

(defn local-minimums-danger [args]
  (let [bordered (surround-with-highs args)
        x-min 1
        y-min 1
        x-max (count (first args))
        y-max (count args)]

    (loop [x x-min
           y y-min
           cnt 0]
      (let [cell (nth (nth bordered y) x)
            min-neighbours (reduce min (neighbours bordered x y))]
        (if (< cell min-neighbours)
            (if (and (= x x-max)
                     (= y y-max))
                (+ cnt (inc cell))
                (if (= x x-max)
                    (recur 1 (inc y) (+ cnt (inc cell)))
                    (recur (inc x) y (+ cnt (inc cell)))))
            (if (and (= x x-max)
                     (= y y-max))
                cnt
                (if (= x x-max)
                    (recur 1 (inc y) cnt)
                    (recur (inc x) y cnt))))))))

(defn -main []
  (let [input (read-input-parsed)]
    (println (local-minimums-danger input))))
