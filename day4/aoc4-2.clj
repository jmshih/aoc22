(ns day4
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn overlap [pair1 pair2]
  (and (<= (first pair1) (first (rest pair2)))
       (>= (first (rest pair1)) (first pair2))))

(defn contains [line]
  (let [pair (str/split line #",")]
    (let [elf1 (get pair 0)
          elf2 (get pair 1)]
      (let [e1secs (map edn/read-string (str/split elf1 #"-"))
            e2secs (map edn/read-string (str/split elf2 #"-"))]
        (if (or (overlap e1secs e2secs) (overlap e2secs e1secs))
          1
          0)))))

(defn -main []
  (with-open [rdr (io/reader "input.txt")]
    (reduce + 0
            (map contains (line-seq rdr)))))
(println (-main))
