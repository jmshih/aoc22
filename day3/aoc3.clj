(ns day3
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as cset]))

(defn get-char-priority [chr]
  (if (< (int chr) (int \a))
    (+ (- (int chr) (int \A)) 27)
    (+ (- (int chr) (int \a)) 1)))

(defn get-priority [group]
  (get-char-priority
    (first
      (reduce cset/intersection
              (map (fn [line] (into #{} line)) group)))))

(defn -main []
  (with-open [rdr (io/reader "input.txt")]
    (reduce + 0
            (map get-priority (partition 3 (line-seq rdr))))))
(println (-main))
