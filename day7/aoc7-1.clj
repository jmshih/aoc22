(ns day7
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defrecord Node [name type size children])

(defn create-tree [lines root cur]
  (do 
    (let [cur-line (first lines)
          rem-lines (rest lines)]
      (cond
        (nil? cur-line) root
        (str/starts-with? cur-line "$ cd /") (recur rem-lines {"/" (Node. "/" "dir" 0 {})} ["/" :children])
        (str/starts-with? cur-line "$ cd ..") (recur rem-lines root (subvec cur 0 (- (count cur) 2)))
        (str/starts-with? cur-line "$ cd") (recur rem-lines root (conj cur (subs cur-line 5) :children))
        (str/starts-with? cur-line "$ ls") (recur rem-lines root cur)
        (str/starts-with? cur-line "dir") (recur 
                                            rem-lines 
                                            (assoc-in 
                                              root 
                                              (conj cur (subs cur-line 4))
                                              (Node. (subs cur-line 4) "dir" 0 {})
                                              )
                                            cur
                                            )
        :else (recur
                rem-lines
                (let [file (str/split cur-line #" ")
                      size (first file)
                      name (last file)
                      ]
                  (assoc-in
                    root
                    (conj cur name)
                    (Node. name "file" (edn/read-string size) {})
                    )
                  )
                cur
                )
        )
      )
    )
  )

(defn get-valid [node]
  (let [node-type (get node :type)
        node-size (get node :size)
        node-name (get node :name)]
    (cond
      (= node-type "file") (list node-size [] [])
      (= node-type "dir") (let [children (map get-valid (vals (get node :children)))
                                sizes (flatten (map first children))
                                dir-sizes (apply concat (map second children))
                                valids (flatten (map last children))
                                cur-size (do (reduce + 0 sizes))]
                            (if (< cur-size 100000)
                              (list cur-size (conj dir-sizes (list node-name cur-size)) (conj valids cur-size))
                              (list cur-size (conj dir-sizes (list node-name cur-size)) valids)
                              )
                            )
      )
    )
  )

(defn -main []
  (with-open [rdr (io/reader "input.txt")]
    ;;(create-tree (line-seq rdr) nil nil)
    (let [result (get-valid (get (create-tree (line-seq rdr) nil nil) "/"))
          root-size (first result)
          dir-sizes (second result)
          valids (last result)
          min-needed (- 30000000 (- 70000000 root-size))]
      (apply min-key #(last %) (filter #(> (last %) min-needed) dir-sizes))
      )
    )
  )
(println (-main))
;;(println (reduce + 0 (last (-main))))
