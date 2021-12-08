(ns aoc.p6
  (:use [aoc.lib :as lib])
  (:gen-class))

(def filename "p6.in")

(defn read-input-parsed []
  (lib/slurp-single-number-seq #"," filename))

(defn progress [fishies]
  (let [birthing-fishies (first fishies)
        born-fishies birthing-fishies
        progressed-fishies (into [] (rest fishies))]
    (update-in (update-in progressed-fishies
                          [6] (fn [base] (+ base birthing-fishies)))
               [8] (fn [base] born-fishies))))

(defn simulate [args day-limit]
  (loop [state (lib/as-count-vec 10 args)
         day-count 0]
      (if (= day-count day-limit)
        (reduce + state)
        (recur (progress state)
               (inc day-count)))))

(defn -main []
  (let [input (read-input-parsed)]
    (println (simulate input 80))
    (println (simulate input 256))))
