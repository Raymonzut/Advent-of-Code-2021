(ns aoc.p7
  (:use [aoc.lib :as lib])
  (:gen-class))

(def filename "p7.in")

(defn read-input-parsed []
  (lib/slurp-single-number-seq #"," filename))

(defn align-cost-basic [positions, alignment]
  (->> positions
       (map (fn [p] (Math/abs (- alignment p))))
       (reduce +)))

(defn align-cost-advanced [positions, alignment]
  (->> positions
       (map (fn [p] (Math/abs (- alignment p))))
       (map (fn [end] (* 0.5 end (inc end))))
       (reduce +)
       (Math/round)))

(defn alignments-within [positions]
  (range (reduce min positions) (inc (reduce max positions))))

(defn minimal-alignment [positions align-cost]
  (loop [left-alignments (alignments-within positions)
         min-align 0
         min-align-cost (align-cost positions (first left-alignments))]
    (if-let [alignment (first left-alignments)]
      (if (< (align-cost positions alignment) min-align-cost)
        (recur (rest left-alignments) alignment (align-cost positions alignment))
        (recur (rest left-alignments) min-align min-align-cost))
      min-align-cost)))

(defn -main []
  (let [input (read-input-parsed)]
    (println (minimal-alignment input align-cost-basic))
    (println (minimal-alignment input align-cost-advanced))))
