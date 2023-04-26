(ns fooheads.numeral-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [fooheads.numeral :refer [digit digits digit-string]]))


(deftest digit-test
  (is (= \0 (digit 0)))
  (is (= \F (digit 15)))
  (is (= \Z (digit 35)))
  (is (= nil (digit 36))))


(deftest digits-test
  (testing "zero"
    (is (= [\0] (digits 0))))

  (testing "binary"
    (is (= [\1 \1 \1 \1] (digits 15 2))))

  (testing "octal"
    (is (= [\1 \7] (digits 15 8))))

  (testing "decimal"
    (is (= [\1 \5] (digits 15 10))))

  (testing "hexadecimal"
    (is (= [\F \F] (digits 255 16)))
    (is (= [\1 \0 \4] (digits 260 16))))

  (testing "padding"
    (is (= [\0 \0 \0 \0 \1 \1 \1 \1] (digits 15 2 8))))

  (testing "with identity as digit function"
    (is (= [1 1 1 1] (digits 15 2 1 identity)))
    (is (= [2 5 5] (digits 255 10 1 identity)))
    (is (= [15 15] (digits 255 16 1 identity))))

  (testing "with a custom digit function"
    (is (= [\γ \β \ε]
           (digits
             214 10 1 (fn [n] (get [\α \β \γ \δ \ε \ζ \η \θ \ι \κ] n)))))))


(deftest digit-string-test
  (is (= "1111" (digit-string 15 2)))
  (is (= "00001111" (digit-string 15 2 8)))

  (testing "with a digit-fn"
    (is (= "1515" (digit-string 255 16 1 identity)))))

