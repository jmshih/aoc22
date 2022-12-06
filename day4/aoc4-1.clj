(ns day4
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn contains [line]
  (let [pair (str/split line #",")]
    (let [elf1 (get pair 0)
          elf2 (get pair 1)]
      (let [e1secs (map edn/read-string (str/split elf1 #"-"))
            e2secs (map edn/read-string (str/split elf2 #"-"))]
        (let [lowest (min (first e1secs) (first e2secs))
              highest (max (first (rest e1secs)) (first (rest e2secs)))]
          (if (or
                (= [lowest highest] e1secs)
                (= [lowest highest] e2secs))
            1
            0))))))

(defn -main []
  (with-open [rdr (io/reader "input.txt")]
    (reduce + 0
            (map contains (line-seq rdr)))))
(println (-main))
