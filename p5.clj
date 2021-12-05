(ns aoc.p5
  (:use [aoc.lib :as lib])
  (:require [clojure.string :as str])
  (:gen-class))

(def filename "p5.in")

(defn parse-point [point]
  (->> point
       (#(str/split % #","))
       (map #(Integer/parseUnsignedInt %))))

(defn parse-line [line]
  (->> line
       (#(str/split % #" -> "))
       (map parse-point)))

(defn read-input-parsed []
  (->> filename
       slurp
       str/split-lines
       (map parse-line)))

(defn generate-range-hv [line]
  (let [[point1 point2] line]
    (cond
      (= (first point1)  (first point2))  (map vector (repeat (first point1))
                                                      (lib/bi-rangi (second point1) (second point2)))
      (= (second point1) (second point2)) (map vector (lib/bi-rangi (first point1) (first point2))
                                                      (repeat (second point1)))
      :else [])))

(defn generate-range-hvd [line]
  (let [[point1 point2] line]
    (cond
      (= (first point1)  (first point2))  (map vector (repeat (first point1))
                                                      (lib/bi-rangi (second point1) (second point2)))
      (= (second point1) (second point2)) (map vector (lib/bi-rangi (first point1) (first point2))
                                                      (repeat (second point1)))
      :else (map vector (lib/bi-rangi (first point1) (first point2))
                        (lib/bi-rangi (second point1)(second point2))))))
             

(defn outer-most [points]
  (loop [left-points points
         curr-max [0 0]]
    (if-let [point (first left-points)]
      (recur (rest left-points) (vector (max (first point) (first curr-max))
                                        (max (second point) (second curr-max))))
      curr-max)))

(defn createBoard [outer-coord]
  (into [] (replicate (inc (second outer-coord))
                      (into [] (replicate (inc (first outer-coord)) 0)))))

(defn point-into [board point]
  (let [[x y] point]
    (update-in board [y] (fn [row] (update-in row [x] inc)))))

(defn apply-points [points board]
  (loop [left-points points
         pointed-board board]
    (if-let [point (first left-points)]
      (recur (rest left-points) (point-into pointed-board point))
      pointed-board)))

(defn dangerous-points [ranges]
  (let [points (reduce concat ranges)
        board (createBoard (outer-most points))
        pointed-board (apply-points points board)]
    (count (filter #(>= % 2) (reduce concat pointed-board)))))

(defn -main []
  (let [input (read-input-parsed)
        hv (filter not-empty (map generate-range-hv input))
        hvd(filter not-empty (map generate-range-hvd input))]
    (println (dangerous-points hv))
    (println (dangerous-points hvd))))
