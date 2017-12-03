(ns advent-of-code-2017.day-2)

(defn row-diff
  [row]
  (- (apply max row)
     (apply min row)))

(defn checksum
  [input]
  (reduce + (map row-diff input)))

(defn parse-input
  [input]
  (map #(let [strings (clojure.string/split % #"\s+")]
          (map (fn [s] (Integer/parseInt s)) strings))
       (clojure.string/split-lines input)))

(defn row-quotient
  [row]
  (first
   (for [dividend row
         divisor row
         :when (and (not= dividend divisor)
                    (= 0 (rem dividend divisor)))]
     (quot dividend divisor))))

(defn checksum-2
  [input]
  (reduce + (map row-quotient input)))
