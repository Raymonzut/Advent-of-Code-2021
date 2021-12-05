(ns aoc.lib)

(defn transpose [rows]
  (loop [columns []
         remainingRows rows]
    (if (empty? (first remainingRows))
      columns
      (recur (conj columns (map first remainingRows)) (map rest remainingRows)))))
