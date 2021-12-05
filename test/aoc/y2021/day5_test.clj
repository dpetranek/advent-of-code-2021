(ns aoc.y2021.day5-test
  (:require [aoc.y2021.day5 :as day5]
            [clojure.test :as t]))

(def ex-lines
  (day5/raw->edn
    ["0,9 -> 5,9"
     "8,0 -> 0,8"
     "9,4 -> 3,4"
     "2,2 -> 2,1"
     "7,0 -> 7,4"
     "6,4 -> 2,0"
     "0,9 -> 2,9"
     "3,4 -> 1,4"
     "0,0 -> 8,8"
     "5,5 -> 8,2"]))

(t/deftest day5
  (t/testing "part1"
    (t/is (= 5 (day5/danger-count ex-lines))))
  (t/testing "part2"
    (t/is (= 12 (day5/accurate-danger-count ex-lines)))))
