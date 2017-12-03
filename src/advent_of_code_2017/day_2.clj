(ns advent-of-code-2017.day-2)

(defn rowDiff
  [row]
  (- (apply max row)
     (apply min row)))

(defn checksum
  [input]
  (reduce + (map rowDiff input)))

(defn parse-input
  [input]
  (map #(let [strings (clojure.string/split % #"\s+")]
          (map (fn [s] (Integer/parseInt s)) strings))
       (clojure.string/split-lines input)))
