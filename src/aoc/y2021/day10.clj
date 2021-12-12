(ns aoc.y2021.day10
  (:require [clojure.string :as str]
            [aoc.util :as ut]))

(def raw "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
")

(defn raw->edn
  [s]
  (str/split-lines s))

(def open->close
  {\(  \)
   \[ \]
   \{ \}
   \< \>})

(def illegal
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(defn checker
  [line]
  (loop [[c & rst] line
         stack []]
    (case c
      (\( \[ \{ \<)
      (recur rst (conj stack c))
      (\> \} \] \))
      (if (= c (open->close (peek stack)))
        (recur rst (pop stack))
        ;; :corrupted
        c)

      ;; end of line
      (if (seq stack)
        ;; :incomplete
        (map open->close (reverse stack))
        ;; :ok
        nil))))

(defn syntax-error-score
  [file]
  (->> (map checker file)
       (keep illegal)
       (reduce +)))

(comment
  (number? )

  (def ex (raw->edn raw))

  (syntax-error-score ex)
  26397

  (def input (raw->edn (ut/read-input "2021/day10.txt")))

  (time (syntax-error-score input))


  ,)

(def completion-score
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn score-completion
  [completion]
  (reduce (fn [score c]
            (+ (* score 5)
               (completion-score c)))
          0
          completion))

(defn autocompleter
  [file]
  (let [scores (->> (map checker file)
                    (filter seq?)
                    (map score-completion)
                    (sort)
                    (into []))]
    (get scores (quot (count scores) 2))))

(comment
  (checker "[({(<(())[]>[[{[]{<()<>>")


  (autocompleter ex)
  288957
  (288957 5566 1480781 995444 294)
  '((\} \} \] \] \) \} \) \])
    (\) \} \> \] \} \))
    (\} \} \> \} \> \) \) \) \))
    (\] \] \} \} \] \} \] \} \>)
    (\] \) \} \>))

  (182193 5366 490493 1839212 582)

  (score-completion "}}]])})]")
  288957

  (score-completion '(\] \) \} \) \] \] \} \}))
  182193


  (time (autocompleter input))
  3404870164

  ,)
