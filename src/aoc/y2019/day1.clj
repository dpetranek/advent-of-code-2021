(ns aoc.y2019.day1
  (:require [aoc.util :as ut]))

(defn raw->edn
  [strings]
  (map #(Integer/parseInt %) strings))

(defn calc-fuel
  [mass]
  (max (- (quot mass 3) 2) 0))


(defn calc-fuel-for-modules
  [module-masses]
  (->> module-masses
       (map calc-fuel)
       (reduce +)))

(comment
  (calc-fuel 12)
  2

  (calc-fuel 14)
  2

  (calc-fuel 1969)
  654

  (calc-fuel 100756)
  33583

  (def input (raw->edn (ut/load-input "2019/1.txt")))

  ;; part1
  (calc-fuel-for-modules input)
  3353880

  )

(defn calc-all-fuel
  "Calculates fuel for mass and fuel for fuel."
  [mass]
  (->> (iterate calc-fuel (calc-fuel mass))
       (take-while pos?)
       (reduce +)))

(defn calc-fuel-for-modules-and-fuel
  [module-masses]
  (->> module-masses
       (map calc-all-fuel)
       (reduce +)))

(comment
  (calc-fuel 2)

  (reduce + (rest (take-while pos? (iterate calc-fuel 14))))
  2


  (->> 1969
       (iterate calc-fuel)
       (take-while pos?)
       (drop 1)
       (reduce +))
  966
  966

  50346
  (->> (calc-fuel 100756)
       (iterate calc-fuel)
       (take-while pos?)
       #_(reduce +))

  ;; part 2
  (calc-fuel-for-modules-and-fuel input)



  )
