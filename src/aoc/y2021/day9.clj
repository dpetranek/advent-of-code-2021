(ns aoc.y2021.day9
  (:require [clojure.string :as str]
            [aoc.util :as ut]
            [clojure.set :as set]))

(def raw "2199943210
3987894921
9856789892
8767896789
9899965678")

(defn raw->edn
  [s]
  (->> (str/split-lines s)
       (map #(str/split % #""))
       (mapv (partial mapv ut/->int))))

(defn transpose
  [seq-of-seqs]
  (apply (partial mapv (fn [& bits] (vec bits)))
         seq-of-seqs))

(defn low-point
  [model [x y]]
  (let [point (get-in model [x y])
        n (get-in model [(dec x) y])
        s (get-in model [(inc x) y])
        e (get-in model [x (inc y)])
        w (get-in model [x (dec y)])
        neighbors (keep identity (set [n s e w]))]
    (when (apply < point (sort neighbors))
      [x y])))

(defn risk-level
  [model low-point]
  (inc (get-in model low-point)))

(defn low-points
  [model]
  (let [coords (for [x (range (count (first (transpose model))))
                     y (range (count (first model)))]
                 [x y])]
    (->> coords
         (keep (partial low-point model)))))

(defn total-risk-level
  [model]
  (->> (low-points model)
       (map (partial risk-level model))
       (reduce +)))

(comment
  (def ex (raw->edn raw))

  (low-points ex)
  ([0 1] [0 9] [2 2] [4 6])

  (total-risk-level ex)
  15

  ;; part1

  (def input (raw->edn (ut/read-input "2021/day9.txt")))

  (time (total-risk-level input))
  607

  ,)

(defn neighbor-coords
  [coord]
  (->> coord
       ((juxt (fn n [[x y]] [(dec x) y])
              (fn s [[x y]] [(inc x) y])
              (fn e [[x y]] [x (inc y)])
              (fn w [[x y]] [x (dec y)])))))

(defn basin
  [model [x y]]
  (loop [to-visit  #{[x y]}
           visited #{}]
      (println to-visit visited)
      (if-let [curr (first (set/difference to-visit visited))]
        (let [_ (println "curr" curr)
              point (get-in model curr)
              neighbors (neighbor-coords curr)
              _ (println "neighbors" neighbors)
              new-dests (reduce (fn [mems addr]
                                  (let [neighb (get-in model addr)]
                                    (if neighb
                                      (if (< point neighb 9)
                                        (conj mems addr)
                                        mems)
                                      mems)))
                                #{}
                                neighbors)]
          (println "new-dests" new-dests)
          (recur (set/union to-visit new-dests) (conj visited curr)))
        visited)))

(defn basins
  [model]
  (->> (low-points model)
       (map (partial basin model))))

(defn assess-basins
  [model]
  (->> (basins model)
       (map count)
       (sort)
       (reverse)
       (take 3)
       (reduce *)))

(comment
  (low-points ex)
  ([0 1] [0 9] [2 2] [4 6])
  (basin ex [0 9])
  #{[1 9] [2 9] [0 9]}

  (basins ex)
  (#{[0 0] [1 0] [0 1]} #{[0 6] [0 5] [1 9] [2 9] [0 9] [1 8] [0 7] [1 6] [0 8]} #{[2 2] [2 3] [2 5] [3 3] [3 4] [3 0] [4 1] [1 4] [1 3] [2 4] [3 1] [2 1] [1 2] [3 2]} #{[4 7] [4 9] [4 6] [4 8] [2 7] [3 6] [4 5] [3 8] [3 7]})

  (assess-basins ex)
  1134

  (assess-basins input)
  900864

  ,)
