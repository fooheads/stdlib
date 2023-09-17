(ns fooheads.test-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [fooheads.stdlib :refer [throw-ex]]
    [fooheads.test :refer [thrown-ex-data]]))


(defn problem-function [x y]
  (let [_x x]
    (throw-ex "Problem: x is {x}" y)))


(deftest thrown-ex-data-test
  (testing "with all ex-data"
    (is (= {:msg "Problem: x is 6" :x 6 :y [:a :b :c]}
           (thrown-ex-data
             (problem-function 6 [:a :b :c])))))

  (testing "with some ex-data"
    (is (= {:msg "Problem: x is 6" :x 6}
           (thrown-ex-data
             [:msg :x]
             (problem-function 6 [:a :b :c])))))

  (testing "expected exception not thrown"
    (is (= {:expected-exception :was-not-thrown}
           (thrown-ex-data
             [:msg :x]
             (identity 6))))))

