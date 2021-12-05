(ns aoc.y2021.day4
  (:require [aoc.util :as ut]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-boards
  [raw-board-strings]
  (loop [[this & remaining] raw-board-strings
         in-progress []
         boards []]
    (cond (nil? this)
          (conj boards in-progress)

          (= this "")
          (recur remaining [] (conj boards in-progress))

          :else
          (let [row (-> this
                        (str/trim)
                        (str/replace #"  " " ")
                        (str/split #" ")
                        (->> (mapv #(Integer/parseInt %))))]
            (recur remaining (conj in-progress row) boards)))))

(defn parse-nums
  [raw-num-strings]
  (-> raw-num-strings
      (str/split #",")
      (->> (map #(Integer/parseInt %)))))

(defn raw->edn
  [strings]
  (let [[raw-nums _ & raw-boards] strings]
    {:numbers (parse-nums raw-nums)
     :boards (parse-boards raw-boards)}))

(defn transpose
  [matrix]
  (apply map vector matrix ))

(defn winner
  "Takes a board and a set of called nums, returns sum of unmarked-numbers if a winner,
  nil if loser."
  [nums board]
  ;; build a sequence of row sets and col sets
  (let [winners (map set (concat board (transpose board)))]
    ;; if the nums are a superset of any of them, that's bingo!
    (when (some (partial set/superset? nums) winners)
      ;; sum up the uncalled numbers
      (reduce + (set/difference (set (flatten board)) nums)))))

(defn play-bingo
  [nums boards]
  (loop [[callout & remaining] nums
         called #{}]
    (let [called' (conj called callout)]
      (if (nil? callout)
        :game-over

        (if-let [bingo! (some (partial winner called') boards)]
          (* callout bingo!)
          (recur remaining (conj called callout)))))))

(comment
  (def input (raw->edn (ut/load-input "4_1.txt")))
  ;; part1

  (play-bingo (:numbers input) (:boards input))
  67716


  ,)

(defn lose-bingo
  "Remove boards until only one remains, then keep calling numbers until it wins."
  [nums boards]
  (loop [[callout & remaining] nums
         prev-called #{}
         boards (set boards)]
    (let [called (conj prev-called callout)]
      (cond (nil? callout)
            :game-over

            (= 1 (count boards))
            (if-let [bingo! (winner called (first boards))]
              ;; last possible win!
              (* callout bingo!)
              ;; last board hasn't won yet...
              (recur remaining called boards))

            :else
            (if-let [bingos (filter (partial winner called) boards)]
              ;; get rid of all the winners
              (recur remaining called (apply disj boards bingos))
              ;; keep going...
              (recur remaining called boards))))))

(comment
  ;; part 2

  (lose-bingo (:numbers input) (:boards input))
  1830

  ,)
