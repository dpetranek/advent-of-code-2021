(ns aoc.y2019.day2
  (:require [aoc.util :as ut]))

(def ex "1,9,10,3,2,3,11,0,99,30,40,50")

(defn raw->edn
  [s]
  (mapv ut/->int (ut/split-comma s)))

(def ex-input (raw->edn ex))

(defn add
  [program pointer]
  (let [[op a-pos b-pos res-pos] (subvec program pointer)
        jmp 4]
    (assert (= op 1))
    [(assoc program res-pos
            (+ (get program a-pos)
               (get program b-pos)))
     (+ pointer jmp)]))

(defn mult
  [program pointer]
  (let [[op a-pos b-pos res-pos] (subvec program pointer)
        jmp 4]
    (assert (= op 2))
    [(assoc program res-pos
            (* (get program a-pos)
               (get program b-pos)))
     (+ pointer jmp)]))

(defn term
  [program _pointer]
  [program nil])

(defn interpret
  [runtime pointer]
  (let [instr (get runtime pointer)]
    (case instr
      1 (add runtime pointer)
      2 (mult runtime pointer)
      99 (term runtime pointer)
      (throw (ex-info "Invalid instruction"
                      {:instruction instr
                       :pointer pointer
                       :runtime runtime})))))

(defn intcode
  [program]
  (loop [runtime program
         pointer 0]
    (let [[next-runtime next-pointer] (interpret runtime pointer)]
      (if next-pointer
        (recur next-runtime next-pointer)
        next-runtime))))

(comment
  ex-input
  [1 9 10 3 2 3 11 0 99 30 40 50]

  (intcode [1,1,1,4,99,5,6,0,99])
  [30 1 1 4 2 5 6 0 99]

  (intcode [2 4 4 5 99 0])
  [2 4 4 5 99 9801]
  (intcode [2 3 0 3 99])
  [2 3 0 6 99]
  (intcode [1 0 0 0 99])
  [2 0 0 0 99]
  (intcode ex-input)
  (intcode ex-input)
  [150 9 10 3 2 3 11 0 99 30 40 50]

  (add [1 9 10 3 2 3 11 0 99 30 40 50] 0)
  (let [program [1 9 10 3 2 3 11 0 99 30 40 50] pointer 0]
    (get program pointer))


  1

  (def input (raw->edn (ut/read-input "2019/2.txt")))
  input

  (let [program (-> input
                    (assoc 1 12)
                    (assoc 2 2))
        [result] (intcode program)]
    result)
  3306701
  3306701

  )

(defn determine-inputs
  [target input]
  (loop [[[noun verb] & inputs] (for [noun (range 1 100) verb (range 1 100)] [noun verb])]
    (let [program (-> input
                      (assoc 1 noun)
                      (assoc 2 verb))
          [result] (intcode program)]
      (if (= result target)
        (+ (* 100 noun) verb)
        (recur inputs)))))

(comment
  (determine-inputs 19690720 input)
  7621

  )
