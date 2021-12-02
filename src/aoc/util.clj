(ns aoc.util
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn load-input
  [filename]
  (->> (slurp (io/resource filename))
       (str/split-lines)))
