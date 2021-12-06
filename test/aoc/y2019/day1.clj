(ns aoc.y2019.day1
  (:require [aoc.y2019.day1 :as day1]
            [clojure.test :as t]))

(t/deftest day5
  (t/testing "part1"
    (t/is (= 2 (day1/calc-fuel 12)))
    (t/is (= 2 (day1/calc-fuel 14)))
    (t/is (= 654 (day1/calc-fuel 1969)))
    (t/is (= 33583 (day1/calc-fuel 100756))))
  (t/testing "part2"
    (t/is (= 2 (day1/calc-all-fuel 14)))
    (t/is (= 50346 (day1/calc-all-fuel 100756)))))
