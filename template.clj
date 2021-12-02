(ns aoc.pn
  (:require [clojure.string :as str])
  (:gen-class))

(def filename "pn.in")

(defn readInputParsed []
  (->> filename
       slurp
       str/split-lines
       (map #(Integer/parseUnsignedInt %))
       ))

(defn helper [args]
  (->> args
       ))

(defn -main []
  (let [input (readInputParsed)]
    (println (helper input))))
