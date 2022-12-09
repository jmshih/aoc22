(ns day7
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.set]))

(defn avg [h t]
  (let [avg-val (/ (+ h t) 2)]
    (if (integer? avg-val)
      avg-val
      h
      )
    )
  )

(defn get-next-h [h dir]
  (let [x (first h)
        y (second h)]
    (case dir
      "U" (list x (inc y))
      "D" (list x (dec y))
      "L" (list (dec x) y)
      "R" (list (inc x) y)
      )
    )
  )

(defn get-next-t [next-h t]
  (let [hx (first next-h)
        hy (second next-h)
        tx (first t)
        ty (second t)]
    (if (and (< (abs (- hx tx)) 2) (< (abs (- hy ty)) 2))
      t
      (list (avg hx tx) (avg hy ty))
      )
    )
  )

(defn move-all [knots]
  (loop [cur-knots knots
         i 1]
    (if (= i 10)
      cur-knots
      (recur
        (assoc cur-knots
               i
               (get-next-t (get cur-knots (dec i)) (get cur-knots i))
               )
        (inc i)
        )
      )
    )
  )

(defn get-positions [knots line]
  (let [move (str/split line #" ")
        dir (first move)
        num-moves (edn/read-string (second move))]
    (loop [cur-knots knots
           moves-left num-moves
           positions (conj #{} (last cur-knots))]
      (do (if (= moves-left 0)
        (list cur-knots positions)
        (let [final (move-all (apply conj [(get-next-h (get cur-knots 0) dir)] (subvec cur-knots 1)))]
          (recur 
            final
            (dec moves-left)
            (conj positions (last final)))
          )
        )
          )
      )
    )
  )

(defn simulate [lines]
  (loop [rem-lines lines
         knots (vec (repeat 10 '(0 0)))
         tail-positions #{'(0 0)}]
    (let [cur-line (first rem-lines)]
      (if (nil? cur-line)
        tail-positions
        (let [next-move (get-positions knots cur-line)
              next-knots (first next-move)
              positions (last next-move)]
          (recur (rest rem-lines) next-knots (clojure.set/union tail-positions positions))
          )
        )
      )
    )
  )

(defn -main []
  (with-open [rdr (io/reader "input.txt")]
    (simulate (line-seq rdr))
    )
  )

(println (count (-main)))
