(ns advent-of-code-2017.day-2-test
  (:require [advent-of-code-2017.day-2 :as sut]
            [clojure.test :as t]))

(def example-input
  '((5 1 9 5)
    (7 5 3)
    (2 4 6 8)))

(t/deftest example
  (t/testing "Example"
    (t/is (= (sut/checksum example-input) 18))))

(def input (slurp (clojure.java.io/resource "day_2.txt")))

(t/deftest answer
  (t/testing "Day 2 solution"
    (t/is (= (sut/checksum (sut/parse-input input)) 45972))))
