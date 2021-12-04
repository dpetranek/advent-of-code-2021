(ns aoc.y2021.day2-test
  (:require [aoc.y2021.day2 :as day2]
            [clojure.test :as t]))

(def ex-cmds [["forward" 5]
              ["down" 5]
              ["forward" 8]
              ["up" 3]
              ["down" 8]
              ["forward" 2]])

(t/deftest day2
  (t/testing "part1"
    (t/is (= 150 (->> ex-cmds
                      (reduce day2/navigate [0 0])
                      (apply *)))))
  (t/testing "part2"
    (t/is (= 900
             (let [{:keys [depth progress]} (reduce day2/navigate-correctly {:progress 0 :depth 0 :aim 0} ex-cmds)]
               (* depth progress))))))
