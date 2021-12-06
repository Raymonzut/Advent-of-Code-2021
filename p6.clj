(ns aoc.p6
  (:require [clojure.string :as str])
  (:gen-class))

(def filename "p6.in")

(defn read-input-parsed []
  (->> filename
       slurp
       str/split-lines
       first
       (#(str/split % #","))
       (map #(Integer/parseUnsignedInt %))))

(defn as-count-vec [fishies]
  (loop [counts (into [] (replicate 9 0))
         left-fishies fishies]
    (if-let [fish (first left-fishies)]
      (recur (update-in counts [fish] inc) (rest left-fishies))
      counts)))

(defn progress [fishies]
  (let [birthing-fishies (first fishies)
        born-fishies birthing-fishies
        progressed-fishies (into [] (rest fishies))]
    (update-in (update-in progressed-fishies
                          [6] (fn [base] (+ base birthing-fishies)))
               [8] (fn [base] born-fishies))))

(defn simulate [args day-limit]
  (loop [state (as-count-vec args)
         day-count 0]
      (if (= day-count day-limit)
        (reduce + state)
        (recur (progress state)
               (inc day-count)))))

(defn -main []
  (let [input (read-input-parsed)]
    (println (simulate input 80))
    (println (simulate input 256))))
