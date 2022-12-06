(ns day2
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn calculate-score [line]
  (let [hand (str/split line #" ")]
    (let [opp (get hand 0)
          you (get hand 1)]
      (case opp
        "A" (case you
               "X" (+ 0 3)
               "Y" (+ 3 1)
               "Z" (+ 6 2))
        "B" (case you
               "X" (+ 0 1)
               "Y" (+ 3 2)
               "Z" (+ 6 3))
        "C" (case you
               "X" (+ 0 2)
               "Y" (+ 3 3)
               "Z" (+ 6 1))))))

(defn -main []
  (with-open [rdr (io/reader "input.txt")]
    (reduce + 0
      (map calculate-score (line-seq rdr)))))
(println (-main))
