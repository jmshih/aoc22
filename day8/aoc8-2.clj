(ns day7
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn convert-char [ch]
  (- (int ch) (int \0))
  )

(defn read-grid [lines]
  (do (vec (map #(vec (map convert-char (vec %))) lines)))
  )

(defn scenic-score-up [grid x y]
  (let [cur-height (get-in grid [y x])]
    (loop [j (dec y)
           cnt 0]
      (cond
        (< j 0) cnt
        (< (get-in grid [j x]) cur-height) (recur (dec j) (inc cnt))
        :else (inc cnt)
        )
      )
    )
  )

(defn scenic-score-down [grid x y]
  (let [cur-height (get-in grid [y x])]
    (loop [j (inc y)
           cnt 0]
      (cond
        (>= j (count grid)) cnt
        (< (get-in grid [j x]) cur-height) (recur (inc j) (inc cnt))
        :else (inc cnt)
        )
      )
    )
  )

(defn scenic-score-left [grid x y]
  (let [cur-height (get-in grid [y x])]
    (loop [i (dec x)
           cnt 0]
      (cond
        (< i 0) cnt
        (< (get-in grid [y i]) cur-height) (recur (dec i) (inc cnt))
        :else (inc cnt)
        )
      )
    )
  )

(defn scenic-score-right [grid x y]
  (let [cur-height (get-in grid [y x])]
    (loop [i (inc x)
           cnt 0]
      (cond
        (>= i (count (get grid 0))) cnt
        (< (get-in grid [y i]) cur-height) (recur (inc i) (inc cnt))
        :else (inc cnt)
        )
      )
    )
  )

(defn scenic-score [grid x y]
  (* (scenic-score-up grid x y) (scenic-score-down grid x y) (scenic-score-left grid x y) (scenic-score-right grid x y))
  )

(defn scenic-scores [grid]
  (for [y (range (count grid))]
    (for [x (range (count (get grid 0)))]
      (let [vis (scenic-score grid x y)]
        (do vis)
        )
      )
    )
  )

(defn -main []
  (with-open [rdr (io/reader "input.txt")]
    (let [grid (read-grid (line-seq rdr))
          scores (scenic-scores grid)]
      (apply max (map #(apply max %) scores))
      )
    )
  )

(println (-main))
