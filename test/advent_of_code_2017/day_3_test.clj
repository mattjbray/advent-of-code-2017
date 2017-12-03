(ns advent-of-code-2017.day-3-test
  (:require [advent-of-code-2017.day-3 :as sut]
            [clojure.test :as t]))

(t/deftest example-1
  (t/testing "Example 1"
    (t/is (= (sut/count-steps 1) 0))))

(t/deftest example-2
  (t/testing "Example 2"
    (t/is (= (sut/count-steps 12) 3))))

(t/deftest example-3
  (t/testing "Example 3"
    (t/is (= (sut/count-steps 23) 2))))

(t/deftest example-4
  (t/testing "Example 4"
    (t/is (= (sut/count-steps 1024) 31))))

(t/deftest solution
  (t/testing "Solution"
    (t/is (= (sut/count-steps 265149) 438))))
