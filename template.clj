(ns aoc.pn
  (:require [clojure.string :as str])
  (:gen-class))

(def filename "pn.in")

(defn read-input-parsed []
  (->> filename
       slurp
       str/split-lines
       (map #(Integer/parseUnsignedInt %))
       ))

(defn helper [args]
  (->> args
       ))

(defn -main []
  (let [input (read-input-parsed)]
    (println (helper input))))
