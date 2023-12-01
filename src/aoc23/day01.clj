(ns aoc23.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(def input (->> (slurp (io/resource "aoc23/day01.txt"))
                  (str/split-lines)))

(defn- tokenize [regex input]
  (map second (re-seq regex input)))

(def token->digit
  {"one" 1 "1" 1
   "two" 2 "2" 2
   "three" 3 "3" 3
   "four" 4 "4" 4
   "five" 5 "5" 5
   "six" 6 "6" 6
   "seven" 7 "7" 7
   "eight" 8 "8" 8
   "nine" 9 "9" 9
   "zero" 0 "0" 0})

(defn- combine-digits [digits]
  (+ (* 10 (first digits)) (last digits)))

(defn- get-numbers [regex input]
  (->> input
       (tokenize regex)
       (map token->digit)
       combine-digits))

(defn- solve-day01 [regex input]
  (->> input
       (map #(get-numbers regex %))
       (apply +)))

(defn part-1
  "AoC 2023 day 1 part 1"
  [input]
  (solve-day01 #"(\d)" input))

(defn part-2
  "AoC 2023 day 1 part 2"
  [input]
  (solve-day01 #"(?=(one|two|three|four|five|six|seven|eight|nine|zero|\d))" input))

(comment
  (def input-1 (str/split-lines "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"))

  (def input-2 (str/split-lines "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"))
  :rcf)

  (comment
    (tokenize #"(?=(one|two|five|\d))" "1blabla3fooo4five")
    (token->digit "six")
    (combine-digits '(2 3 4 6))
    (combine-digits '(4))
    (get-numbers #"(\d)" "1blabla3fooo4five")
    (part-1 input)
    (part-2 input)
    :rcf)