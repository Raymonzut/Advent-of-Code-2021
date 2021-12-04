(ns aoc.p4
  (:require [clojure.string :as str])
  (:gen-class))

(def filename "p4.in")

(def MARKED -1)

(def fileContents
  (->> filename
       slurp
       str/split-lines))


(defn parseNumbers [row]
  (->> row
       (#(str/split % #","))
       (map #(Integer/parseUnsignedInt %))))

(defn rowToNums [row]
  (map #(Integer/parseUnsignedInt %) row))

(defn parseCard [cardText]
  (->> cardText
       (map #(str/split % #" "))
       (map #(filter not-empty %))
       (map rowToNums)))

(defn parseBingoCards [rows]
  (loop [input rows
         cards []]
    (let [newCard (take 5 input)]
      (if (< (count input) 5)
        cards
        (recur (drop 6 input) (conj cards (parseCard newCard)))))))


(defn transpose [rows]
  (loop [columns []
         remainingRows rows]
    (if (empty? (first remainingRows))
      columns
      (recur (conj columns (map first remainingRows)) (map rest remainingRows)))))


(defn hasHorizontalBingo [markedCard]
  (some #(every? (partial = MARKED) %) markedCard))

(defn hasVerticalBingo [markedCard]
  (some #(every? (partial = MARKED) %) (transpose markedCard)))


(defn markWith [bingoNumber cellNumber]
  (if (= bingoNumber cellNumber)
    MARKED
    cellNumber))

(defn markRow [bingoNumber row]
  (map (partial markWith bingoNumber) row))

(defn markCard [bingoNumber rows]
  (map (partial markRow bingoNumber) rows))

(defn markCards [bingoNumber cards]
  (map (partial markCard bingoNumber) cards))

(defn scoreRow [row]
  (reduce + (filter (partial not= MARKED) row)))

(defn hasBingo [card]
  (or (hasHorizontalBingo card)
      (hasVerticalBingo card)))

(defn scoreCard [card]
  (if (hasBingo card)
    (reduce + (map scoreRow card))
    0))

(defn simulateBingoNightPart1 []
  (loop [drawnBingoNumbers (parseNumbers (first fileContents))
         bingoCards (parseBingoCards (drop 2 fileContents))]
    (let [drawnNumber (first drawnBingoNumbers)
          markedBingoCards (markCards drawnNumber bingoCards)
          roundScores (map scoreCard markedBingoCards)]
      (if (some #(> % 0) roundScores)
        (* drawnNumber (first (filter #(> % 0) roundScores)))
        (recur (rest drawnBingoNumbers) markedBingoCards)))))

(defn simulateBingoNightPart2 []
  (loop [drawnBingoNumbers (parseNumbers (first fileContents))
         bingoCards (parseBingoCards (drop 2 fileContents))
         winningCards []]
    (let [drawnNumber (first drawnBingoNumbers)
          markedBingoCards (markCards drawnNumber bingoCards)
          remainingCards (filter #(not (hasBingo %)) markedBingoCards)
          newWinners (map vector (repeat drawnNumber) (filter hasBingo markedBingoCards))]
      (if (= 0 (count drawnBingoNumbers))
        (* (first (last winningCards))
           (scoreCard (second (last winningCards))))

        (recur (rest drawnBingoNumbers)
               remainingCards
               (concat winningCards newWinners))))))

(defn -main []
  (println (simulateBingoNightPart1))
  (println (simulateBingoNightPart2)))
