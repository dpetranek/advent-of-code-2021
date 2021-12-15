(ns aoc.y2021.day13
  (:require [clojure.string :as str]
            [aoc.util :as ut]
            [clojure.set :as set]))

(def raw "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
")

(defn raw->edn
  [s]
  (let [[dots folds] (map str/split-lines (str/split s #"\n\n"))]
    [(->> dots
          (map #(str/split % #","))
          (map (partial mapv ut/->int))
          (into #{}))
     (map #(-> (str/replace % #"fold along " "")
               (str/split #"=")
               (->> (apply (fn [axis intercept] [(keyword axis) (ut/->int intercept)]))))
          folds)]))

(defn max-x
  [dots]
  (apply max (map first dots)))

(defn max-y
  [dots]
  (apply max (map second dots)))

(defn print-paper
  [dots]
  (let [width  (inc (max-x dots))
        height (inc (max-y dots))
        grid   (partition width
                          (for [y (range height) x (range width)]
                            [x y]))]
    (println (apply str (repeat width "@")))
    (doseq [row grid]
      (println (apply str (map #(if (dots %) "#" ".") row))))))

(defn move-y
  [dots intercept]
  (let [{affected true unaffected false} (group-by (fn [[_x y]] (> y intercept)) dots)]
    (->> affected
         (map (fn [[x y]]
                (let [fold-dist (- y intercept)]
                  [x (- intercept fold-dist)])))
         (into (set unaffected)))))

(defn move-x
  [dots intercept]
  (let [{affected true unaffected false} (group-by (fn [[x _y]] (> x intercept)) dots)]
    (->> affected
         (map (fn [[x y]]
                (let [fold-dist (- x intercept)]
                  [(- intercept fold-dist) y])))
         (into (set unaffected)))))

;; get affected dots
;; translate them to the new coords
;; merge with the old
(defn fold
  [dots [axis intercept]]
  (let [{affected true unaffected false}
        (group-by (fn [[x y]]
                    (let [coord ({:x x :y y} axis)]
                      (> coord intercept))) dots)]
    (->> affected
         (map (fn [[x y]]
                (let [coord ({:x x :y y} axis)
                      fold-dist (- coord intercept)]
                  (case axis
                    :x [(- intercept fold-dist) y]
                    :y [x (- intercept fold-dist)]))))
         (into (set unaffected)))))

(defn fold-all
  [[dots folds]]
  (print-paper (reduce (fn [paper instr] (fold paper instr))
                 dots
                 folds)))


(comment
  (def ex (raw->edn raw))
  [#{[8 4] [3 4] [3 0] [9 0] [0 13] [8 10] [0 14] [1 10] [4 11] [4 1] [0 3] [10 12] [10 4] [6 10] [6 12] [2 14] [9 10] [6 0]} ([:y 7] [:x 5])]

  {false [[8 4] [3 4] [3 0] [9 0] [4 1] [0 3] [10 4] [6 0]],
   true [[0 13] [8 10] [0 14] [1 10] [4 11] [10 12] [6 10] [6 12] [2 14] [9 10]]}
  (reduce (fn [[still moving] [_x y :as dot]]
            (if (> y 7)
              [still (conj moving dot)]
              [(conj still dot) moving]))
          [[] []]
          (first ex))

  (count (fold (first ex) (first (second ex))))

  (apply max (map first (first ex)))
  10

  (apply max (map second (first ex)))
  14

  (def fold1 (fold (first ex) [:y 7]))
  (def fold2 (fold fold1 [:x 5]))
  (sort-by second (sort-by first fold2))
  ([0 0] [1 0] [2 0] [3 0] [4 0]
   [0 1] [4 1]
   [0 2] [4 2]
   [0 3] [4 3]
   [0 4] [1 4] [2 4] [3 4] [4 4])
  (sort-by second (sort-by first fold1))
  ([0 0] [2 0] [3 0] [6 0] [9 0]
   [0 1] [4 1]
   [6 2] [10 2]
   [0 3] [4 3]
   [1 4] [3 4] [6 4] [8 4] [9 4] [10 4])

  (print-paper fold1)
  (print-paper fold2)

  (def input (raw->edn (ut/read-input "2021/day13.txt")))

  (count (fold (first input) (first (second input))))
  747

  (fold-all input)


  ,
  )
