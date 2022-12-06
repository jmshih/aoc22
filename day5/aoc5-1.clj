(ns day5
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn parse-bucket [bucket]
  (do (let [chr (first (rest bucket))]
    (if (= chr \ )
      nil
      chr))))

(defn parse-line [line]
  (do (mapv parse-bucket (partition-all 4 line))))

(defn parse-containers [lines]
  (let [parsed (parse-line (first lines))]
    (if (= (get parsed 0) \1)
      (map (fn [chr] '()) parsed)
      (map 
        (fn [container chr] (if (nil? chr) container (conj container chr)))
        (parse-containers (rest lines))
        parsed))))

(defn parse-move [line containers]
  (if (str/starts-with? line "move")
    (let [instr (str/split line #" ")]
      (let [num-move (edn/read-string (get instr 1))
            from (- (edn/read-string (get instr 3)) 1)
            to (- (edn/read-string (get instr 5)) 1)]
        (let [to-container (get containers to)
              from-container (get containers from)]
          (assoc 
            (assoc containers to (reduce conj to-container (reverse (take num-move from-container))))
            from
            (drop num-move from-container)))))
    containers
  )
)

(defn move [lines containers]
  (do (if (empty? lines)
    containers
    (recur (rest lines) (parse-move (first lines) containers)))))

(defn -main []
  (with-open [rdr (io/reader "input.txt")]
    (let [lines (vec (line-seq rdr))]
      (move lines (vec (parse-containers lines))))
  )
)
(println (-main))
