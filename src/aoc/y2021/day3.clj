(ns aoc.y2021.day3
  (:require [aoc.util :as ut]))

(defn char->int
  [char]
  (Integer/parseInt (str char)))

(def ex-diagnosis
  (map (comp #(mapv char->int %) seq)
       ["00100"
        "11110"
        "10110"
        "10111"
        "10101"
        "01111"
        "00111"
        "11100"
        "10000"
        "11001"
        "00010"
        "01010"]))

(defn gamma-rate
  [bits]
  (let [{one 1 zed 0} (frequencies bits)]
    (if (>= one zed) 1 0)))

(defn epsilon-rate
  [bits]
  (let [{one 1 zed 0} (frequencies bits)]
    (if (<= zed one) 0 1)))

(defn binary->decimal
  [numstr]
  (Integer/parseInt numstr 2))

(comment
  (let [bits (->> ex-diagnosis
                  ;; transpose them
                  (apply (partial map (fn [& bits] bits))))
        gamma   (->> bits (map gamma-rate) (reduce str) binary->decimal)
        epsilon (->> bits (map epsilon-rate) (reduce str) binary->decimal)]
    (* gamma epsilon))
  198
  (0 1 0 0 1)
  (1 0 1 1 0)


  ,)


(defn transpose
  [seq-of-seqs]
  (apply (partial map (fn [& bits] (vec bits)))
         seq-of-seqs))

(defn power-consumption
  [diagnosis]
  (let [bits    (transpose diagnosis)
        gamma   (->> bits (map gamma-rate) (reduce str) binary->decimal)
        epsilon (->> bits (map epsilon-rate) (reduce str) binary->decimal)]
    (* gamma epsilon)))

(comment
  (def input (map (comp #(mapv char->int %) seq) (ut/load-input "3_1.txt")))

  ;; Part1
  (power-consumption input)
  845186

  ,)

(defn raw->diagnosis
  [input]
  (map seq input))

(defn oxygen-generator-rating
  [diagnosis]
  (loop [rows diagnosis
         idx  0]
    (let [candidates (map #(nth % idx) rows)
          criteria   (gamma-rate candidates)
          search     (filter #(= criteria (nth % idx)) rows)]
      #_(println "o2" idx criteria (count search))
      (if (= 1 (count search))
        (first search)
        (recur search (inc idx))))))

(defn carbon-scrubber-rating
  [diagnosis]
  (loop [rows diagnosis
         idx  0]
    (let [candidates (map #(nth % idx) rows)
          criteria   (epsilon-rate candidates)
          search     (filter #(= criteria (nth % idx)) rows)]
      #_(println "co" idx criteria (count search))
      (if (= 1 (count search))
        (first search)
        (recur search (inc idx))))))

(defn life-support-rating
  [diagnosis]
  (let [o2 (->> (oxygen-generator-rating diagnosis)
                (reduce str)
                binary->decimal)
        co (->> (carbon-scrubber-rating diagnosis)
                (reduce str)
                binary->decimal)]
    (* o2 co)))

(comment
  ex-diagnosis

  (loop [rows ex-diagnosis
         idx  0]
    (let [candidates (map #(nth % idx) rows)
          criteria   (epsilon-rate candidates)
          search     (filter #(= criteria (nth % idx)) rows)]
      (if (= 1 (count search))
        (first search)
        (recur search (inc idx)))))
  (->> [0 1 0 1 0]
       (reduce str)
       (binary->decimal))
  10
  (->> [1 0 1 1 1]
       (reduce str)
       (binary->decimal))
  23


  (oxygen-generator-rating ex-diagnosis)
  [1 0 1 1 1]
  (carbon-scrubber-rating ex-diagnosis)
  [0 1 0 1 0]

  (life-support-rating ex-diagnosis)
  230

  ;; Part2

  (life-support-rating input)
  4636702


  ,)
