(ns aoc.y2021.day3-test
  (:require [aoc.y2021.day3 :as day3]
            [clojure.test :as t]))


(def ex-diagnosis
  (map (comp #(mapv day3/char->int %) seq)
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

(t/deftest day3
  (t/testing "part1"
    (t/is (= 198 (day3/power-consumption ex-diagnosis))))
  (t/testing "part2"
    (t/is (= 230 (day3/life-support-rating ex-diagnosis)))))
