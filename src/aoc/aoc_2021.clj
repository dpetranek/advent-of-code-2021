(ns aoc.aoc-2021
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(comment
  (def depths [199
               200
               208
               210
               200
               207
               240
               269
               260
               263])

  (->> depths
       (partition 2 1)
       (map (fn [[a b]]
              (if (< a b)
                :increased
                :decreased))))
  (:increased :increased :increased :decreased :increased :increased :increased :decreased :increased)

  (->> depths
       (partition 3 1)
       (map (partial apply +))
       (partition 2 1)
       (map (fn [[a b]]
              (cond
                (< a b) :increased
                (> a b) :decreased
                (= a b) :no-change
                :else (throw (Exception. "bad"))))))
  (:increased :no-change :decreased :increased :increased :increased :increased)

  (607 618 618 617 647 716 769 792)
  ((199 200 208) (200 208 210) (208 210 200) (210 200 207) (200 207 240) (207 240 269) (240 269 260) (269 260 263))





  (slurp (io/resource "test.txt"))


  (->> (slurp (io/resource "1_1.txt"))
       (str/split-lines)
       (map #(Integer/parseInt %))
       (partition 2 1)
       (map (fn [[a b]]
              (if (< a b)
                :increased
                :decreased)))

       (frequencies))
  {:increased 1752, :decreased 247}

  (->> (slurp (io/resource "1_1.txt"))
       (str/split-lines)
       (map #(Integer/parseInt %))
       (partition 3 1)
       (map (partial apply +))
       (partition 2 1)
       (map (fn [[a b]]
              (cond
                (< a b) :increased
                (> a b) :decreased
                (= a b) :no-change
                :else (throw (Exception. "bad")))))
       (frequencies))
  {:increased 1781, :decreased 183, :no-change 33}





  ,,,)


(defn load-input
  [filename]
  (->> (slurp (io/resource filename))
       (str/split-lines)))

(comment
  (def in2 (load-input "2_1.txt"))

  (def data2 (->> in2
                  (map (fn [s] (str/split s #" ")))
                  (map (fn [[cmd magnitude]] [cmd (Integer/parseInt magnitude)]))))


  (->> data2
       (reduce (fn [[progress depth] [cmd dist]]
                 (case cmd
                   "forward" [(+ progress dist) depth]
                   "backward" [(- progress dist) depth]
                   "up" [progress (- depth dist)]
                   "down" [progress (+ depth dist)])
                 )
               [0 0])
       (apply *))
  2120749
  [2053 1033]

  (def ex2 [["forward" 5]
            ["down" 5]
            ["forward" 8]
            ["up" 3]
            ["down" 8]
            ["forward" 2]])

  (defn navigate [{:keys [progress depth aim] :as pos} [cmd dist]]
    (println (pr-str pos) (pr-str [cmd dist]))
    (case cmd
      "forward" (-> pos
                    (assoc :progress (+ progress dist))
                    (assoc :depth (+ depth (* aim dist))))
      "up" (-> pos
               #_(assoc :depth (- depth dist))
               (assoc :aim (- aim dist)))
      "down" (-> pos
                 #_(assoc :depth (+ depth dist))
                 (assoc :aim (+ aim dist)))))

  (->> ex2
       (reduce navigate {:depth 0 :progress 0 :aim 0}))
  {:depth 60, :progress 15, :aim 10}
  {:depth 60, :progress 15}

  (let [{:keys [depth progress]} (reduce navigate {:depth 0 :progress 0 :aim 0} data2)]
    (* depth progress))
  2138382217
  {:depth 1041589, :progress 2053, :aim 1033}
  {:depth 3099, :progress 2053, :aim 1033}
  (* 3099 2053)
  6362247





  ,,,)
