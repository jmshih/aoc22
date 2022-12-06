(ns day6
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn -main []
  (with-open [rdr (io/reader "input.txt")]
    (let [chrs (vec (slurp rdr))]
      (loop [i 0
             group []
             remains chrs]
        (do (if (nil? (first remains)) -1 (if (and (= 14 (count group)) (= 14 (count (set group))))
          i
          (recur (inc i) (conj (if (= 14 (count group)) (subvec group 1) group) (first remains)) (rest remains)))))))))

(println (-main))

(defn alternative []
  (with-open [rdr (io/reader "input.txt")]
    (let [chrs (vec (slurp rdr))]
      (first 
        (filter (fn [e] (= 15 (count e)))
                (map-indexed (fn [i el] (conj (set el) (+ i 14))) (partition 14 1 chrs)))))))

;;(println (alternative))
