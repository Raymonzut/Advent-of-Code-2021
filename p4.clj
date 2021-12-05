(ns aoc.p4
  (:use [aoc.lib :as lib])
  (:require [clojure.string :as str])
  (:gen-class))

(def filename "p4.in")

(def marked-cell -1)

(def file-contents
  (->> filename
       slurp
       str/split-lines))


(defn parse-numbers [row]
  (->> row
       (#(str/split % #","))
       (map #(Integer/parseUnsignedInt %))))

(defn parse-card-row [row]
  (map #(Integer/parseUnsignedInt %) row))

(defn parse-card [card-text]
  (->> card-text
       (map #(str/split % #" "))
       (map #(filter not-empty %))
       (map parse-card-row)))

(defn parse-cards [rows]
  (loop [input rows
         cards []]
    (let [new-card (take 5 input)]
      (if (< (count input) 5)
        cards
        (recur (drop 6 input) (conj cards (parse-card new-card)))))))


(defn bingo-horizontal? [marked-card]
  (some #(every? (partial = marked-cell) %) marked-card))

(defn bingo-vertical? [marked-card]
  (some #(every? (partial = marked-cell) %) (lib/transpose marked-card)))


(defn mark-with [bingo-number cell-number]
  (if (= bingo-number cell-number)
    marked-cell
    cell-number))

(defn mark-row [bingo-number row]
  (map (partial mark-with bingo-number) row))

(defn mark-card [bingo-number rows]
  (map (partial mark-row bingo-number) rows))

(defn mark-cards [bingo-number cards]
  (map (partial mark-card bingo-number) cards))

(defn score-of-row [row]
  (reduce + (filter (partial not= marked-cell) row)))

(defn bingo? [card]
  (or (bingo-horizontal? card)
      (bingo-vertical? card)))

(defn score-of-card [card]
  (if (bingo? card)
    (reduce + (map score-of-row card))
    0))

(defn simulate-bingonight-part1 []
  (loop [numbers-drawn (parse-numbers (first file-contents))
         bingo-cards (parse-cards (drop 2 file-contents))]
    (let [number-drawn (first numbers-drawn)
          marked-bingo-cards (mark-cards number-drawn bingo-cards)
          round-scores (map score-of-card marked-bingo-cards)]
      (if (some #(> % 0) round-scores)
        (* number-drawn (first (filter #(> % 0) round-scores)))
        (recur (rest numbers-drawn) marked-bingo-cards)))))

(defn simulate-bingonight-part2 []
  (loop [numbers-drawn (parse-numbers (first file-contents))
         bingo-cards (parse-cards (drop 2 file-contents))
         winning-cards []]
    (let [number-drawn (first numbers-drawn)
          marked-bingo-cards (mark-cards number-drawn bingo-cards)
          remaining-cards (filter #(not (bingo? %)) marked-bingo-cards)
          new-winners (map vector (repeat number-drawn) (filter bingo? marked-bingo-cards))]
      (if (= 0 (count numbers-drawn))
        (* (first (last winning-cards))
           (score-of-card (second (last winning-cards))))

        (recur (rest numbers-drawn)
               remaining-cards
               (concat winning-cards new-winners))))))

(defn -main []
  (println (simulate-bingonight-part1))
  (println (simulate-bingonight-part2)))
