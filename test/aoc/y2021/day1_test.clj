(ns aoc.y2021.day1-test
  (:require [aoc.y2021.day1 :as day1]
            [clojure.test :as t]))

(def ex-depths [199
                200
                208
                210
                200
                207
                240
                269
                260
                263])

(t/deftest day1
  (t/testing "part1"
    (t/is (= 7 (:increased (day1/depth-measurement-report ex-depths)))))
  (t/testing "part2"
    (t/is (= 5 (:increased (day1/cleaned-depth-measurement-report ex-depths))))))
