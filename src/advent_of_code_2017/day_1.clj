(ns advent-of-code-2017.day-1)

(defn digits
  "Get a list of digits from a string"
  [string]
  (map #(Character/digit % 10) string))

(defn captcha-n
  "Find the sum of all digits that match the digit n places up the list."
  [n ds]
  (let [length (count ds)
        lookup (fn [i] (let [j (+ n i)
                             j (if (< j 0) (+ j length) j)
                             j (if (>= j length) (- j length) j)]
                         (nth ds j)))
        result (reduce (fn [acc x]
                         (let [acc (if (= x (lookup (:i acc)))
                                     (update acc :sum + x)
                                     acc)]
                           (update acc :i inc)))
                       {:i 0 :sum 0}
                       ds)]
    (:sum result)))

(defn captcha-next
  "Find the sum of all digits that match the next digit in the list."
  [input]
  (captcha-n 1 (digits input)))

(defn captcha-halfway
  [input]
  (let [ds (digits input)
        n (/ (count ds) 2)]
    (captcha-n n ds)))
