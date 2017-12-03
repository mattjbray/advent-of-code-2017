(ns advent-of-code-2017.day-1-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2017.day-1 :refer :all]))

(def input
  (clojure.string/trim-newline
   (slurp (clojure.java.io/resource "day_1.txt"))))

; Part One

(deftest example-1-1
  (testing "Example 1.1"
    (is (= (captcha-next "1122") 3))))

(deftest example-1-2
  (testing "Example 1.2"
    (is (= (captcha-next "1111") 4))))

(deftest example-1-3
  (testing "Example 1.3"
    (is (= (captcha-next "1234") 0))))

(deftest example-1-4
  (testing "Example 1.4"
    (is (= (captcha-next "91212129") 9))))

(deftest answer-1
  (testing "Part One"
    (is (= (captcha-next input) 1341))))

; Part Two

(deftest example-2-1
  (testing "Example 2.1"
    (is (= (captcha-halfway "1212") 6))))

(deftest example-2-2
  (testing "Example 2.2"
    (is (= (captcha-halfway "1221") 0))))

(deftest example-2-3
  (testing "Example 2.3"
    (is (= (captcha-halfway "123425") 4))))

(deftest example-2-4
  (testing "Example 2.4"
    (is (= (captcha-halfway "123123") 12))))

(deftest example-2-5
  (testing "Example 2.5"
    (is (= (captcha-halfway "12131415") 4))))

(deftest answer-2
  (testing "Part Two"
    (is (= (captcha-halfway input) 1348))))
