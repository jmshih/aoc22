(ns day7
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.set]))

(defn parse-line [line]
  (let [parts (str/split line #" ")
        op (first parts)
        amt (edn/read-string (second parts))]
    (if (= op "noop")
      (list 0)
      (list 0 amt)
      )
    )
  )

(defn combine [result draw-cycles]
  (map
    #(map vector %1 %2)
    result
    draw-cycles
    )
  )

(defn overlaps [el]
  (let [pixels (first el)
        cycle (second el)]
    (if (and (>= cycle (dec pixels)) (<= cycle (inc pixels)))
      "#"
      "."
      )
    )
  )

(defn -main []
  (with-open [rdr (io/reader "input.txt")]
    (vec
      (reductions
        +
        1
        (flatten
          (map parse-line (vec (line-seq rdr)))
          )
        )
      )
    )
  )
(defn join [result]
  (str/join
    "\n"
    (map
      #(str/join "" %)
      result
      )
    )
  )
(println
  (let [result (partition 40 (-main))
        draw-cycles (repeat 6 (range 40))]
    (join
      (map
        #(vec (map overlaps %))
        (combine result draw-cycles)
        )
      )
    )
  )
