(ns aoc.lib)

(defn transpose [rows]
  (loop [columns []
         remaining-rows rows]
    (if (empty? (first remaining-rows))
      columns
      (recur (conj columns (map first remaining-rows))
             (map rest remaining-rows)))))
