(ns aoc.y2021.day4
  (:require [aoc.util :as ut]
            [clojure.string :as str]
            [clojure.set :as set]))

(def ex-bingo
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")

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

(defn transpose
  [matrix]
  (apply map vector matrix ))

(defn winner
  "Takes a board and a set of called nums, returns sum of unmarked-numbers if a winner,
  nil if loser."
  [nums board]
  (let [winners (map set (concat board (transpose board)))]
    (when (some (partial set/superset? nums) winners)
      ;; sum up the uncalled numbers
      (apply + (set/difference (set (mapcat identity board))
                               nums)))))

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
  ;; winning: any of 5 cols, any of 5 rows
  ;; set of rows and cols

  (let [[raw-nums _ & raw-boards] (str/split-lines ex-bingo)]
    (def ex-nums (parse-nums raw-nums))
    (def ex-boards (parse-boards raw-boards)))

  (play-bingo ex-nums ex-boards)
  4512


  ,)


(comment
  (let [[raw-nums _ & raw-boards] (ut/load-input "4_1.txt")]
    (def bingo-nums (parse-nums raw-nums))
    (def boards (parse-boards raw-boards)))

  ;; part1

  (play-bingo bingo-nums boards)
  67716


  ,)

(defn lose-bingo
  "Remove boards until only one remains, then keep calling numbers until it wins."
  [nums boards]
  (loop [[callout & remaining] nums
         prev-called #{}
         boards (set boards)]
    (let [called (conj prev-called callout)]
      (println callout (count called) (count boards))
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
  (lose-bingo ex-nums ex-boards)
  1924

  (lose-bingo bingo-nums boards)
  1830
  :game-over




  )
