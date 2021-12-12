(ns aoc.y2021.day8
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.walk :as walk]
            [clojure.core.logic :as logic]
            [aoc.util :as ut]))

(def raw "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(defn raw->edn
  [s]
  (->> (str/split-lines s)
       (map #(str/split % #" \| "))
       (map (partial map #(str/split % #" ")))))

(def number->segments
  {0 #{\a \b \c \e \f \g}
   1 #{\c \f}
   2 #{\a \c \d \e \g}
   3 #{\a \c \d \f \g}
   4 #{\b \c \d \f}
   5 #{\a \b \d \f \g}
   6 #{\a \b \d \e \f \g}
   7 #{\a \c \f}
   8 #{\a \b \c \d \e \f \g}
   9 #{\a \b \c \d \f \g}})

(def sig-grp-size->number
  {6 [9 6 0], 3 7, 2 1, 4 4, 5 [2 3 5], 7 8})

(def segment->numbers
  (->> number->segments
       (mapcat (fn [[k v]] (map #(vector % k) v)))
       (reduce (fn [m [segment number]] (update m segment (fnil conj #{}) number)) {})))

;;   0:      1:      2:      3:      4:
;;  aaaa    ....    aaaa    aaaa    ....
;; b    c  .    c  .    c  .    c  b    c
;; b    c  .    c  .    c  .    c  b    c
;;  ....    ....    dddd    dddd    dddd
;; e    f  .    f  e    .  .    f  .    f
;; e    f  .    f  e    .  .    f  .    f
;;  gggg    ....    gggg    gggg    ....

;;   5:      6:      7:      8:      9:
;;  aaaa    aaaa    aaaa    aaaa    aaaa
;; b    .  b    .  .    c  b    c  b    c
;; b    .  b    .  .    c  b    c  b    c
;;  dddd    dddd    ....    dddd    dddd
;; .    f  e    f  .    f  e    f  .    f
;; .    f  e    f  .    f  e    f  .    f
;;  gggg    gggg    ....    gggg    gggg

(defn count-unique
  [in]
  (->> in
       (map second)
       (mapcat (partial map count))
       (map sig-grp-size->number)
       (filter number?)
       count))

(comment
  (def ex (raw->edn raw))
  ex

  (count-unique ex)
  26

  (def input (raw->edn (ut/read-input "2021/day8.txt")))
  input
  (count-unique input)
  362


  ,)


(def b1 '[3 - - - - 5 - 1 -
          - 7 - - - 6 - 3 -
          1 - - - 9 - - - -
          7 - 8 - - - - 9 -
          9 - - 4 - 8 - - 2
          - 6 - - - - 5 - 1
          - - - - 4 - - - 6
          - 4 - 7 - - - 2 -
          - 2 - 6 - - - - 3])

(defn prep
  [board]
  (map #(partition 3 %)
       (partition 9 board)))

(defn print-board
  [board]
  (let [row-sep (apply str (repeat 37 "-"))]
    (println row-sep)
    (dotimes [row (count board)]
      (print "| ")
      (doseq [subrow (nth board row)]
        (doseq [cell (butlast subrow)]
          (print (str cell " ")))
        (print (str (last subrow) " | ")))
      (println)
      (when (zero? (mod (inc row) 3))
        (println row-sep)))))

(defn rows
  [board sz]
  (partition sz board))

(defn row-for
  [board index sz]
  (nth (rows board sz) (/ index 9)))

(defn column-for
  [board index sz]
  (let [col (mod index sz)]
    (map #(nth % col) (rows board sz))))

(defn subgrid-for
  [board i]
  (let [rows (rows board 9)
        sgcol (/ (mod i 9) 3)
        sgrow (/ (/ i 9) 3)
        grp-col (column-for (mapcat #(partition 3 %) rows) sgcol 3)
        grp (take 3 (drop (* 3 (int sgrow)) grp-col))]
    (flatten grp)))

(defn numbers-present-for
  [board i]
  (set
    (concat (row-for board i 9)
            (column-for board i 9)
            (subgrid-for board i))))

(defn possible-placements
  [board index]
  (set/difference #{1 2 3 4 5 6 7 8 9}
                  (numbers-present-for board index)))

(defn index [coll] (cond (map? coll) (seq coll) (set? coll) (map vector coll coll) :else (map vector (iterate inc 0) coll)))
(defn index-when [pred coll] (for [[i v] (index coll) :when (pred v)] i))
(defn solve
  [board]
  (if-let [[i & _] (and (some '#{-} board)
                        (index-when '#{-} board))]
    (flatten (map #(solve (assoc board i %))
                  (possible-placements board i)))
    board))

(comment
  (print-board (prep b1))

  (row-for b1 1 9)
  (3 - - - - 5 - 1 -)
  (column-for b1 0 9)
  (3 - 1 7 9 - - - -)

  (subgrid-for b1 0)
  (3 - - - 7 - 1 - -)

  (numbers-present-for b1 1)
  #{7 1 4 - 6 3 2 5}

  (numbers-present-for (assoc b1 1 8) 1)
  #{7 1 4 - 6 3 2 5 8}

  (set/difference #{7 1 4 6 3 2 5 8 9}
                  (numbers-present-for b1 1))
  #{9 8}
  #{8}

  (-> b1 solve prep print-board)
  nil

  ,)

(defn lvar?
  "Detrmines if a value represents a logic variable."
  [x]
  (boolean
    (when (symbol? x)
      (re-matches #"^\?.*" (name x)))))

(defn satisfy1
  [l r knowledge]
  (let [L (get knowledge l l)
        R (get knowledge r r)]
    (cond (= L R) knowledge
          (lvar? L) (assoc knowledge L R)
          (lvar? R) (assoc knowledge R L)
          :else nil)))

(defn satisfy
  [l r knowledge]
  (let [L (get knowledge l l)
        R (get knowledge r r)]
    (cond (not knowledge) nil
          (= L R)         knowledge
          (lvar? L)       (assoc knowledge L R)
          (lvar? R)       (assoc knowledge R L)
          (every? seq? [L R]) (satisfy (rest L)
                                       (rest R)
                                       (satisfy (first L)
                                                (first R)
                                                knowledge))

          :else           nil)))

(comment
  (lvar? 2)

  (satisfy1 '?x '?y {})
  {?x ?y}
  {?something 2}
  {?something 2}

  (->> {}
       (satisfy1 '?x '?y)
       (satisfy1 '?x 1))
  {?x ?y, ?y 1}

  (satisfy '(1 2 3) '(1 ?something 3) {})
  {?something 2}


  (satisfy (number->segments 7) '#{\a \b \d} {})
  nil

  ,)

(defn subst
  [term binds]
  (walk/prewalk
    (fn [expr]
      (if (lvar? expr)
        (or (binds expr) expr)
        expr))
    term))

(comment
  (subst '(1 ?x 3) '{?x 2})
  (1 2 3)

  (subst '(1 ?x 3) '{})
  (1 ?x 3)
  (subst '(1 ?x 3) '{?x ?y})
  (1 ?y 3)

  )

(defn meld
  [term1 term2]
  (->> {}
       (satisfy term1 term2)
       (subst term1)))

(comment
  (meld '(1 ?x 3) '(1 2 ?y))
  (1 2 3)
  (meld '(1 ?x) '(?y (?y 2)))
  (1 (1 2))


  ,)

(comment
  (logic/run* [answer]
    (logic/== answer 5))
  (5)

  (logic/run* [val1 val2]
    (logic/== {:a val1 :b 2}
              {:a 1 :b val2}))
  ([1 2])

  (logic/run* [x y]
    (logic/== x y))
  ([_0 _0])

  (logic/run* [q]
    (logic/== q 1)
    (logic/== q 2))
  ()

  (logic/run* [george]
    (logic/conde
      [(logic/== george :born)]
      [(logic/== george :unborn)]))
  (:born :unborn)

  (logic/run* [q]
    (logic/membero q [1 2 3])
    (logic/membero q [2 3 4]))
  (2 3)

  (let [a [1]]
    (logic/run* [q]
      (logic/== q a)))
  ([1])
  (1)

  ;; the goal is to unify a segment with a signal
  ;; we can figure it out by constraining
  ;; we know that it can be this /or/ that /or/ that
  ;; gradually winnow options until we're down to one

  (let [a [1]]
    (logic/run* [q]
      (logic/membero q [])
      (logic/membero q [0 2 3 4 5 6 7 8 9])))
  (1 2 3)

  number->segments
  {0 #{\a \b \c \e \f \g} ,
   7 #{\a \c \f} ,
   1 #{\c \f} ,
   4 #{\b \c \d \f} ,
   6 #{\a \b \d \e \f \g} ,
   3 #{\a \c \d \f \g} ,
   2 #{\a \c \d \e \g} ,
   9 #{\a \b \c \d \f \g} ,
   5 #{\a \b \d \f \g} ,
   8 #{\a \b \c \d \e \f \g}}
  segment->numbers
  {\a #{0 7 6 3 2 9 5 8} ,
   \b #{0 4 6 9 5 8} ,
   \c #{0 7 1 4 3 2 9 8} ,
   \e #{0 6 2 8} ,
   \f #{0 7 1 4 6 3 9 5 8} ,
   \g #{0 6 3 2 9 5 8} ,
   \d #{4 6 3 2 9 5 8}}

  (let [[[signals digits]] ex]
    (->> signals
         (map (fn [signals]
                (let [possible-nums (signal-count->number (count signals))
                      possible-segments (map (juxt (constantly (set signals)) identity number->segments) possible-nums)]
                  possible-segments)))

         (filter #(= 1 (count %)))))
  '(([#{\a \b \c \d \e \f \g} 8 #{\a \b \c \d \e \f \g}])
    ([#{\a \b \d} 7 #{\a \c \f}])
    ([#{\a \b \e \f} 4 #{\b \c \d \f}])
    ([#{\a \b} 1 #{\c \f}]))

  (logic/run* [q]
    (logic/membero q #{\a \b \c \d \e \f \g}))



  ,)
