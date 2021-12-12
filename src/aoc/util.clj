(ns aoc.util
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input
  [filename]
  (slurp (io/resource filename)))

(defn load-input
  [filename]
  (->> (slurp (io/resource filename))
       (str/split-lines)))

(defn split-comma
  [s]
  (str/split (str/trim s) #","))

(defn ->int
  [s]
  (Integer/parseInt s))

(defn n  [[x y]] [x (dec y)])
(defn s  [[x y]] [x (inc y)])
(defn e  [[x y]] [(inc x) y])
(defn w  [[x y]] [(dec x) y])
(defn nw [[x y]] [(dec x) (dec y)])
(defn ne [[x y]] [(inc x) (dec y)])
(defn sw [[x y]] [(dec x) (inc y)])
(defn se [[x y]] [(inc x) (inc y)])
