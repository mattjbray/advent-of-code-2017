(ns advent-of-code-2017.day-1-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2017.core :refer :all]))

(deftest example-1
  (testing "Example 1"
    (is (= (captcha "1122") 3))))

(deftest example-2
  (testing "Example 2"
    (is (= (captcha "1111") 4))))

(deftest example-3
  (testing "Example 3"
    (is (= (captcha "1234") 0))))

(deftest example-4
  (testing "Example 4"
    (is (= (captcha "91212129") 9))))
