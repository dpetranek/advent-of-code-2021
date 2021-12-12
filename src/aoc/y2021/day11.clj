(ns aoc.y2021.day11
  (:require [clojure.string :as str]
            [aoc.util :as ut]
            [clojure.set :as set]))

(def raw "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")

(defn raw->edn
  [s]
  (->> (str/split-lines s)
       (map #(str/split % #""))
       (mapv (partial mapv ut/->int))))

(defn octopus
  [octopi [x y]]
  (get-in octopi [y x]))

(defn find-flashes
  [octopi]
  (set
    (for [y (range (count octopi))
          x (range (count (first octopi)))
          :when (> (octopus octopi [x y]) 9)]
      [x y])))

(defn neighbors
  [coord]
  ((juxt ut/n ut/s ut/e ut/w ut/nw ut/ne ut/se ut/sw) coord))

(defn power
  ([octopi]
   (mapv (partial mapv inc) octopi))
  ([octopi [x y]]
   (if (octopus octopi [x y])
     (update-in octopi [y x] inc)
     octopi)))

(defn reset
  [octopi [x y]]
  (if (octopus octopi [x y])
     (assoc-in octopi [y x] 0)
     octopi))

(defn print-octopi!
  [octopi]
  (println (reduce str (repeat 10 "-")))
  (doseq [line octopi]
    (println (reduce str line))))


(defn tick
  [octopi]
  (loop [octopi  (power octopi)
         flashed #{}]
    (if-let [flashpoints (not-empty (set/difference (find-flashes octopi) flashed))]
      (let [affected (mapcat neighbors flashpoints)]
        (recur (reduce power octopi affected)
               (into flashed flashpoints)))
      [(reduce reset octopi flashed) (count flashed)])))

(defn all-flash?
  [octopi]
  (every? (partial every? zero?) octopi))

(comment
  (all-flash? [[0 0] [0 0]])
  true
  )

(defn count-flashes
  [octopi ticks]
  (loop [step    0
         flashes 0
         octopi  octopi]
    (if (= step ticks)
      flashes
      (let [[octopi* new-flashes] (tick octopi)]
        (recur (inc step) (+ flashes new-flashes) octopi*)))))

(defn find-synchronization
  [octopi ticks]
  (loop [step    0
         flashes 0
         octopi  octopi]
    (if (or (all-flash? octopi) (= step ticks))
      step
      (let [[octopi* new-flashes] (tick octopi)]
        (recur (inc step) (+ flashes new-flashes) octopi*)))))

(comment

  (def ex (raw->edn raw))
  (def ex1 (raw->edn "11111
19991
19191
19991
11111"))

  (tick ex1)
  [[3 4 5 4 3]
   [4 0 0 0 4]
   [5 0 0 0 5]
   [4 0 0 0 4]
   [3 4 5 4 3]]


  (count-flashes ex 100)
  1656

  (find-synchronization ex 500)
  195

  (def input (raw->edn "4743378318
4664212844
2535667884
3273363861
2282432612
2166612134
3776334513
8123852583
8181786685
4362533174
"))

  (count-flashes input)
  1741


  ,)
