(ns advent-of-code-2017.day-2-test
  (:require [advent-of-code-2017.day-2 :as sut]
            [clojure.test :as t]))

(def input (slurp (clojure.java.io/resource "day_2.txt")))

; Part One

(def example-1-input
  '((5 1 9 5)
    (7 5 3)
    (2 4 6 8)))

(t/deftest example-1
  (t/testing "Example 1"
    (t/is (= (sut/checksum example-1-input) 18))))

(t/deftest answer-1
  (t/testing "Day 2 part 1 solution"
    (t/is (= (sut/checksum (sut/parse-input input)) 45972))))

; Part Two


(def example-2-input
  '((5 9 2 8)
    (9 4 7 3)
    (3 8 6 5)))

(t/deftest example-2
  (t/testing "Example 2"
    (t/is (= (sut/checksum-2 example-2-input) 9))))

(t/deftest answer-2
  (t/testing "Day 2 part 2 solution"
    (t/is (= (sut/checksum-2 (sut/parse-input input)) 326))))
