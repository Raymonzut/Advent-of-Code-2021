(ns aoc.lib)

(defn bi-rangi [p1 p2]
  "Range that works both ways, inclusive end"
  (if (< p1 p2)
    (range p1 (inc p2))
    (reverse (range p2 (inc p1)))))

(defn transpose [rows]
  (loop [columns []
         remaining-rows rows]
    (if (empty? (first remaining-rows))
      columns
      (recur (conj columns (map first remaining-rows))
             (map rest remaining-rows)))))
