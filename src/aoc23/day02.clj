(ns aoc23.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (->> (slurp (io/resource "aoc23/day02.txt"))
                (str/split-lines)))

(defn- parse-game-number [game-text]
  (->> game-text
      (re-find #"Game (\d+)")
      rest
      first
      read-string))

(defn- parse-cube-result [result-text]
  (let [[count-str color] (rest (re-find #"\s*(\d+) (red|green|blue)" result-text))]
    {color (read-string count-str)}))

(defn- parse-game-set [set-text]
  (let [elements (str/split set-text #",")
        [m1 m2 m3] (map parse-cube-result elements)]
    (merge {"red" 0 "green" 0 "blue" 0} m1 m2 m3)))


(defn- parse-game-line [line]
  (let [[game-text sets-text] (str/split line #":")
        sets-coll (str/split sets-text #";")]
    {:game (parse-game-number game-text)
     :sets (map parse-game-set sets-coll)}))

(defn- is-cube-value-valid? [bag-val set-val]
  (let [sval (if (nil? set-val) 0 set-val)
        bval (if (nil? bag-val) 0 bag-val)]
    (<= sval bval)))

(defn- is-game-set-valid? [bag game-set]
  (and (is-cube-value-valid? (bag "red") (game-set "red"))
       (is-cube-value-valid? (bag "green") (game-set "green"))
       (is-cube-value-valid? (bag "blue") (game-set "blue"))))

(defn- is-game-valid? [bag game-map]
  (every? #(is-game-set-valid? bag %) (game-map :sets)))

(defn- get-valid-games [bag coll]
  (filter #(is-game-valid? bag %) coll))

(defn- get-game-number [game-map]
  (game-map :game))

(defn part-1 [input bag]
  (let [game-coll (map parse-game-line input)]
    (->> game-coll
        (get-valid-games bag)
        (map get-game-number)
         (reduce + 0))))

(defn- make-minimum-bag [s1 s2]
  (cond-> s1
    (< (s1 "blue") (s2 "blue")) (assoc "blue" (s2 "blue"))
    (< (s1 "green") (s2 "green")) (assoc "green" (s2 "green"))
    (< (s1 "red") (s2 "red")) (assoc "red" (s2 "red"))))

(defn part-2 [input]
  (let [game-coll (map parse-game-line input)]
    (->> game-coll
         (map :sets)
         (map #(reduce make-minimum-bag %))
         (map #(apply * (vals %)))
         (reduce + 0))))


(comment
  (def input-1 (str/split-lines "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"))
  (def bag1 {"red" 12 "green" 13 "blue" 14})

  (re-find #"Game (\d+): (\d+) (red|green|blue)" "Game 5: 3 red")
  (let [[game-text sets-text] (str/split "Game 5: 3 red, 4 blue; 5 red, 11 blue" #":")
        [set1-text set2-text] (str/split sets-text #";")]
    [game-text set1-text set2-text])
  
  (parse-game-line (first input-1))
  (get-game-number "Game 2")
  (parse-game-set " 3 blue, 4 green, 5 red")
  (parse-game-set " 3 blue, 9 red")
  (parse-cube-result "3 blue")
  (is-cube-value-valid? 5 nil)
  (parse-game-line (nth input-1 2))
  (part-1 input-1 bag1)
  (part-1 input bag1)
  (make-minimum-bag {"red" 0 "blue" 3 "green" 5} {"green" 4 "red" 3 "blue" 4})
  (part-2 input-1)
  (part-2 input)
  :rcf)