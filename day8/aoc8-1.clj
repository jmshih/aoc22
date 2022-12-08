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

(defn visible-up? [grid x y]
  (let [cur-height (get-in grid [y x])]
    (do
      (if (= y 0)
        true
        (reduce #(and %1 (< %2 cur-height)) true (map #(get-in grid [% x]) (range y)))
        )
      )
    )
  )

(defn visible-down? [grid x y]
  (let [cur-height (get-in grid [y x])]
    (if (= y (dec (count grid)))
      true
      (reduce #(and %1 (< %2 cur-height)) true (map #(get-in grid [% x]) (range (inc y) (count grid))))
      )
    )
  )

(defn visible-left? [grid x y]
  (let [cur-height (get-in grid [y x])]
    (if (= x 0)
      true
      (reduce #(and %1 (< %2 cur-height)) true (map #(get-in grid [y %]) (range x)))
      )
    )
  )

(defn visible-right? [grid x y]
  (let [cur-height (get-in grid [y x])]
    (if (= x (dec (count (get grid 0))))
      true
      (reduce #(and %1 (< %2 cur-height)) true (map #(get-in grid [y %]) (range (inc x) (count (get grid 0)))))
      )
    )
  )

(defn is-visible? [grid x y]
  (do (or (visible-up? grid x y) (visible-down? grid x y) (visible-left? grid x y) (visible-right? grid x y)))
  )

(defn find-visible [grid]
  (for [y (range (count grid))]
    (for [x (range (count (get grid 0)))]
      (let [vis (is-visible? grid x y)]
        (do vis)
        )
      )
    )
  )

(defn -main []
  (with-open [rdr (io/reader "input.txt")]
    (let [grid (read-grid (line-seq rdr))]
      (count (filter #(= % true) (flatten (find-visible grid))))
      )
    )
  )

(println (-main))
