(ns day1
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn count-calories [group]
  (reduce + 0 (map edn/read-string group)))

(defn process-lines [cur-group rem-lines]
  (cond
    (empty? rem-lines) (do (conj '() (count-calories cur-group)))
    (str/blank? (first rem-lines)) (do (conj (process-lines '() (rest rem-lines)) (count-calories cur-group)))
    :else (do (process-lines (conj cur-group (first rem-lines)) (rest rem-lines)))))

(defn -main []
  (with-open [rdr (io/reader "input.txt")]
    (println (reduce + 0 (take-last 3 (sort (process-lines '() (line-seq rdr))))))))
(-main)
