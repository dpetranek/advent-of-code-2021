(ns aoc.y2021.day6
  (:require [aoc.util :as ut]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn raw->edn
  [s]
  (map #(Integer/parseInt %) (str/split (str/trim s) #",")))

(def ex-fishes
  (raw->edn "3,4,3,1,2"))

(defn tick
  [school]
  (reduce (fn [new-school fish]
            (if (zero? fish)
              (conj new-school 6 8)
              (conj new-school (dec fish))))
          []
          school))

(defn simulate-lanternfish
  [days school]
  (->> school
       (iterate tick)
       (drop days)
       first
       count))

(comment
  ;; for each zero, append an 8 to the end
  ;; also, replace each 0 with a

  (simulate-lanternfish 80 ex-fishes)
  5934
  (simulate-lanternfish 256 ex-fishes)

  (simulate-lanternfish 18 ex-fishes)
  26

  (count (first (drop 80 (iterate tick ex-fishes))))
  5934
  26
  (count [6 8 1 1 3 0 2 2 4 6 8 1 1 3 4 6 6 8 6 8 1 5 7 0 0 2])
  26



  (def input (raw->edn (slurp (io/resource "2021/day6.txt"))))
  input

  ;; part1
  (simulate-lanternfish 80 input)
  391888
  376508

  )

(defn tick2
  [school-freqs]
  (let [{births -1 :as step} (reduce-kv (fn [step countdown fishes]
                                         (assoc step (dec countdown) fishes))
                                       {} school-freqs)]
    (-> step
        (dissoc -1)
        (update 8 (fnil (partial + (or births 0)) 0))
        (update 6 (fnil (partial + (or births 0)) 0)))))

(defn simulate-lanternfish-efficiently
  [days school]
  (->> (frequencies school)
       (iterate tick2)
       (drop days)
       first
       (reduce-kv (fn [total _ v] (+ total v)) 0)))

(comment
  (take 18 (iterate tick ex-fishes))
  (map frequencies '((3 4 3 1 2)
                     [2 3 2 0 1]
                     [1 2 1 6 8 0]
                     [0 1 0 5 7 6 8]
                     [6 8 0 6 8 4 6 5 7]
                     [5 7 6 8 5 7 3 5 4 6]
                     [4 6 5 7 4 6 2 4 3 5]
                     [3 5 4 6 3 5 1 3 2 4]
                     [2 4 3 5 2 4 0 2 1 3]
                     [1 3 2 4 1 3 6 8 1 0 2]
                     [0 2 1 3 0 2 5 7 0 6 8 1]
                     [6 8 1 0 2 6 8 1 4 6 6 8 5 7 0]
                     [5 7 0 6 8 1 5 7 0 3 5 5 7 4 6 6 8]
                     [4 6 6 8 5 7 0 4 6 6 8 2 4 4 6 3 5 5 7]
                     [3 5 5 7 4 6 6 8 3 5 5 7 1 3 3 5 2 4 4 6]
                     [2 4 4 6 3 5 5 7 2 4 4 6 0 2 2 4 1 3 3 5]
                     [1 3 3 5 2 4 4 6 1 3 3 5 6 8 1 1 3 0 2 2 4]
                     [0 2 2 4 1 3 3 5 0 2 2 4 5 7 0 0 2 6 8 1 1 3]))
  (= '({3 2, 4 1, 1 1, 2 1}
       {2 2, 3 1, 0 1, 1 1}
       {1 2, 2 1, 6 1, 8 1, 0 1}
       {0 2, 1 1, 5 1, 7 1, 6 1, 8 1}
       {6 3, 8 2, 0 1, 4 1, 5 1, 7 1}
       {5 3, 7 2, 6 2, 8 1, 3 1, 4 1}
       {4 3, 6 2, 5 2, 7 1, 2 1, 3 1}
       {3 3, 5 2, 4 2, 6 1, 1 1, 2 1}
       {2 3, 4 2, 3 2, 5 1, 0 1, 1 1}
       {1 3, 3 2, 2 2, 4 1, 6 1, 8 1, 0 1}
       {0 3, 2 2, 1 2, 3 1, 5 1, 7 1, 6 1, 8 1}
       {6 4, 8 3, 1 2, 0 2, 2 1, 4 1, 5 1, 7 1}
       {5 4, 7 3, 0 2, 6 3, 8 2, 1 1, 3 1, 4 1}
       {4 4, 6 5, 8 2, 5 3, 7 2, 0 1, 2 1, 3 1}
       {3 4, 5 5, 7 2, 4 3, 6 3, 8 1, 1 1, 2 1}
       {2 4, 4 5, 6 2, 3 3, 5 3, 7 1, 0 1, 1 1}
       {1 4, 3 5, 5 2, 2 3, 4 3, 6 2, 8 1, 0 1}
       {0 4, 7 1, 1 3, 4 2, 6 1, 3 3, 2 5, 5 2, 8 1})
     '({3 2, 4 1, 1 1, 2 1}
       {2 2, 3 1, 0 1, 1 1, 8 0, 6 0}
       {1 2, 2 1, 0 1, 7 0, 5 0, 8 1, 6 1}
       {0 2, 1 1, 6 1, 4 0, 7 1, 5 1, 8 1}
       {0 1, 5 1, 3 0, 6 3, 4 1, 7 1, 8 2}
       {4 1, 2 0, 5 3, 3 1, 6 2, 7 2, 8 1}
       {3 1, 1 0, 4 3, 2 1, 5 2, 6 2, 7 1, 8 0}
       {0 0, 7 0, 1 1, 4 2, 6 1, 3 3, 2 1, 5 2, 8 0}
       {0 1, 7 0, 1 1, 4 2, 6 0, 3 2, 2 3, 5 1, 8 0}
       {0 1, 7 0, 1 3, 4 1, 6 1, 3 2, 2 2, 5 0, 8 1}
       {0 3, 7 1, 1 2, 4 0, 6 1, 3 1, 2 2, 5 1, 8 1}
       {0 2, 7 1, 1 2, 4 1, 6 4, 3 0, 2 1, 5 1, 8 3}
       {0 2, 7 3, 1 1, 4 1, 6 3, 3 1, 2 0, 5 4, 8 2}
       {0 1, 7 2, 1 0, 4 4, 6 5, 3 1, 2 1, 5 3, 8 2}
       {0 0, 7 2, 1 1, 4 3, 6 3, 3 4, 2 1, 5 5, 8 1}
       {0 1, 7 1, 1 1, 4 5, 6 2, 3 3, 2 4, 5 3, 8 0}
       {0 1, 7 0, 1 4, 4 3, 6 2, 3 5, 2 3, 5 2, 8 1}
       {0 4, 7 1, 1 3, 4 2, 6 1, 3 3, 2 5, 5 2, 8 1})
     '({3 2, 4 1, 1 1, 2 1}
       {2 2, 3 1, 0 1, 1 1}
       {1 2, 2 1, -1 1, 0 1}
       {0 2, 1 1, -2 1, -1 1}
       {-1 2, 0 1, -3 1, -2 1}
       {-2 2, -1 1, -4 1, -3 1}
       {-3 2, -2 1, -5 1, -4 1}
       {-4 2, -3 1, -6 1, -5 1}
       {-5 2, -4 1, -7 1, -6 1}
       {-6 2, -5 1, -8 1, -7 1}
       {-7 2, -6 1, -9 1, -8 1}
       {-8 2, -7 1, -10 1, -9 1}
       {-9 2, -8 1, -11 1, -10 1}
       {-10 2, -9 1, -12 1, -11 1}
       {-11 2, -10 1, -13 1, -12 1}
       {-12 2, -11 1, -14 1, -13 1}
       {-13 2, -12 1, -15 1, -14 1}
       {-14 2, -13 1, -16 1, -15 1})
     '({3 2, 4 1, 1 1, 2 1}
       {2 2, 3 1, 0 1, 1 1, 7 0, 5 0}
       {1 2, 2 1, 0 1, 6 0, 4 0, 7 1, 5 1}
       {0 2, 1 1, 5 1, 3 0, 6 1, 4 1, 7 1}
       {0 1, 4 1, 2 0, 5 3, 3 1, 6 1, 7 2}
       {3 1, 1 0, 4 3, 2 1, 5 2, 6 2, 7 1}
       {2 1, 0 0, 3 3, 1 1, 4 2, 5 2, 6 1, 7 0}
       {1 1, 2 3, 0 1, 3 2, 4 2, 5 1, 6 0, 7 0}
       {0 1, 1 3, 2 2, 3 2, 4 1, 5 1, 6 0, 7 1}
       {0 3, 1 2, 2 2, 3 1, 4 1, 5 1, 6 1, 7 1}
       {0 2, 1 2, 2 1, 3 1, 4 1, 5 4, 6 1, 7 3}
       {0 2, 1 1, 2 1, 3 1, 4 4, 5 3, 6 3, 7 2}
       {0 1, 1 1, 2 1, 3 4, 4 3, 5 5, 6 2, 7 2}
       {0 1, 1 1, 2 4, 3 3, 4 5, 5 3, 6 2, 7 1}
       {0 1, 1 4, 2 3, 3 5, 4 3, 5 3, 6 1, 7 1}
       {0 4, 1 3, 2 5, 3 3, 4 3, 5 2, 6 1, 7 1}
       {0 3, 1 5, 2 3, 3 3, 4 2, 5 5, 6 1, 7 4}
       {0 5, 1 3, 2 3, 3 2, 4 5, 5 4, 6 4, 7 3}))


  (take 18 (iterate tick2 (frequencies ex-fishes)))
  ({3 2, 4 1, 1 1, 2 1} {2 2, 3 1, 0 1, 1 1, 8 0, 6 0} {1 2, 2 1, 0 1, 7 0, 5 0, 8 1, 6 1} {0 2, 1 1, 6 1, 4 0, 7 1, 5 1, 8 1} {0 1, 5 1, 3 0, 6 3, 4 1, 7 1, 8 2} {4 1, 2 0, 5 3, 3 1, 6 2, 7 2, 8 1} {3 1, 1 0, 4 3, 2 1, 5 2, 6 2, 7 1, 8 0} {0 0, 7 0, 1 1, 4 2, 6 1, 3 3, 2 1, 5 2, 8 0} {0 1, 7 0, 1 1, 4 2, 6 0, 3 2, 2 3, 5 1, 8 0} {0 1, 7 0, 1 3, 4 1, 6 1, 3 2, 2 2, 5 0, 8 1} {0 3, 7 1, 1 2, 4 0, 6 1, 3 1, 2 2, 5 1, 8 1} {0 2, 7 1, 1 2, 4 1, 6 4, 3 0, 2 1, 5 1, 8 3} {0 2, 7 3, 1 1, 4 1, 6 3, 3 1, 2 0, 5 4, 8 2} {0 1, 7 2, 1 0, 4 4, 6 5, 3 1, 2 1, 5 3, 8 2} {0 0, 7 2, 1 1, 4 3, 6 3, 3 4, 2 1, 5 5, 8 1} {0 1, 7 1, 1 1, 4 5, 6 2, 3 3, 2 4, 5 3, 8 0} {0 1, 7 0, 1 4, 4 3, 6 2, 3 5, 2 3, 5 2, 8 1} {0 4, 7 1, 1 3, 4 2, 6 1, 3 3, 2 5, 5 2, 8 1})
  ({3 2, 4 1, 1 1, 2 1} {2 2, 3 1, 0 1, 1 1} {1 2, 2 1, -1 1, 0 1} {0 2, 1 1, -2 1, -1 1} {-1 2, 0 1, -3 1, -2 1} {-2 2, -1 1, -4 1, -3 1} {-3 2, -2 1, -5 1, -4 1} {-4 2, -3 1, -6 1, -5 1} {-5 2, -4 1, -7 1, -6 1} {-6 2, -5 1, -8 1, -7 1} {-7 2, -6 1, -9 1, -8 1} {-8 2, -7 1, -10 1, -9 1} {-9 2, -8 1, -11 1, -10 1} {-10 2, -9 1, -12 1, -11 1} {-11 2, -10 1, -13 1, -12 1} {-12 2, -11 1, -14 1, -13 1} {-13 2, -12 1, -15 1, -14 1} {-14 2, -13 1, -16 1, -15 1})
  ({3 2, 4 1, 1 1, 2 1}
   {2 2, 3 1, 0 1, 1 1, 7 0, 5 0}
   {1 2, 2 1, 0 1, 6 0, 4 0, 7 1, 5 1}
   {0 2, 1 1, 5 1, 3 0, 6 1, 4 1, 7 1}
   {0 1, 4 1, 2 0, 5 3, 3 1, 6 1, 7 2}
   {3 1, 1 0, 4 3, 2 1, 5 2, 6 2, 7 1}
   {2 1, 0 0, 3 3, 1 1, 4 2, 5 2, 6 1, 7 0}
   {1 1, 2 3, 0 1, 3 2, 4 2, 5 1, 6 0, 7 0}
   {0 1, 1 3, 2 2, 3 2, 4 1, 5 1, 6 0, 7 1}
   {0 3, 1 2, 2 2, 3 1, 4 1, 5 1, 6 1, 7 1}
   {0 2, 1 2, 2 1, 3 1, 4 1, 5 4, 6 1, 7 3}
   {0 2, 1 1, 2 1, 3 1, 4 4, 5 3, 6 3, 7 2}
   {0 1, 1 1, 2 1, 3 4, 4 3, 5 5, 6 2, 7 2}
   {0 1, 1 1, 2 4, 3 3, 4 5, 5 3, 6 2, 7 1}
   {0 1, 1 4, 2 3, 3 5, 4 3, 5 3, 6 1, 7 1}
   {0 4, 1 3, 2 5, 3 3, 4 3, 5 2, 6 1, 7 1}
   {0 3, 1 5, 2 3, 3 3, 4 2, 5 5, 6 1, 7 4}
   {0 5, 1 3, 2 3, 3 2, 4 5, 5 4, 6 4, 7 3})

  37




  ({3 2, 4 1, 1 1, 2 1}
   {2 2, 3 1, 1 1, 8 1, 6 1}
   {1 2, 2 1, 7 1, 5 1, 8 1, 6 1})



  {2 2, 3 1, 0 1, 1 1}
  (frequencies ex-fishes)
  {3 2, 4 1, 1 1, 2 1}



  (frequencies ex-fishes)
  (reduce-kv (fn [a k v] (assoc a k (dec v))) {} {3 2, 4 1, 1 1, 2 1})
  {3 1, 4 0, 1 0, 2 0}

  (-> {}
      (update 8 (fnil #(+ 2 %) 0)))
  {8 2}
  {8 5}

  {8 3}
  {8 2}

  {8 1}
  {3 2, 4 1, 1 1, 2 1})


(defn fish-count
  [birthday fish deathday]
  (quot (- deathday birthday) 6))

(defn children-birthdays
  [deathday]
  (fn [birthday]
    (loop [bdays []
           today birthday]
      (let [next-bday (+ today (if (empty? bdays) 9 7))]
        (if (<= next-bday deathday)
          (recur (conj bdays next-bday) next-bday)
          bdays)))))


(defn calculate-lanternfish
  [days school]
  (loop [fish-count (count school)
         children (->> school
                       ;; convert each fish to it's birthday
                       (map #(- % 8))
                       (mapcat (children-birthdays days)))]
    (if (seq children)
      (recur (+ fish-count (count children))
             (mapcat (children-birthdays days)
                     children))
      fish-count)))


(comment
  ;;part2

  ;; this approach is very slow, let's do it recursively
  ;; we know the number of days, we can figure out how many ancestors it will produce
  ;; do we actually sim the fish or do we count the fish?

  ;; take a fish, calculate how many fish it will produce, add that number of 8s to the end
  ;; keep track of the number of days, and decrement until the end

  (let [fish 3 days 18]
    (quot (- days fish) 6))
  2
  (->> ex-fishes
       ;; convert each fish to it's birthday
       (map #(- % 8))
       (mapcat (partial children-birthdays 18))
       (mapcat (partial children-birthdays 18))
       (mapcat (partial children-birthdays 18)))
  ()
  ([] [] [] [] [] [] [])
  (13 14 13 11 18 18 12)
  ([13] [] [] [14] [] [13] [] [] [11 18] [18] [] [12] [] [])
  (4 11 18 5 12 4 11 18 2 9 16 3 10 17)
  ([4 11 18] [5 12] [4 11 18] [2 9 16] [3 10 17])

  ([4 11 18] [5 12] [4 11 18] [2 9 16] [3 10 17])

  (loop [school (count ex-fishes)
         children (->> ex-fishes
                       ;; convert each fish to it's birthday
                       (map #(- % 8))
                       (mapcat (partial children-birthdays 18)))]
    (if (seq children)
      (recur (+ school (count children)) (mapcat (partial children-birthdays 18) children))
      school))
  26


  (calculate-lanternfish 256 ex-fishes)


  (calculate-lanternfish 80 ex-fishes)
  5934

  (time (simulate-lanternfish 150 ex-fishes))
  26
  (time (simulate-lanternfish-efficiently 256 ex-fishes))
  26984457539
  26
  34


  26
  (time (calculate-lanternfish 150 ex-fishes))
  26

  (let [school ex-fishes
        days 18]
    (loop [school (count school)
           children (->> school
                         ;; convert each fish to it's birthday
                         (map #(- % 8))
                         (mapcat (partial children-birthdays days)))]
      (if (seq children)
        (recur (+ school (count children))
               (mapcat (children-birthdays days)
                       children))
        school)))



  2
  (- 18 -5)
  23
  (children-birthdays -5 18)
  [4 11 18]
  [3 9 15 21]
  [3 9 15 21]
  (-5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17)
  3

  ;; (1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8)
  ;;  3 2 1 0 6 5 4 3 2 1 0 6 5 4 3 2 1 0
  ;;  _ _ _ 8 7 6 5 4 3 2 1 0 6 5 4 3 2 1
  ;;  _ _ _ _ _ _ _ _ _ _ _ 8 7 6 5 4 3 2
  ;;  _ _ _ _ _ _ _ _ _ _ 8 7 6 5 4 3 2 1
  ;;  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 8


  (simulate-lanternfish-efficiently 256 input)
  1754597645339
  ,)
