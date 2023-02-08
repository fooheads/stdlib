(ns fooheads.shape-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [fooheads.shape :refer [intersection]]))


(deftest intersection-test
  (testing "map intersection with empty maps"
    (is (= {} (intersection {} {}))))

  (testing "map intersection are the common keys"
    (is (= {:a 1 :b 2} (intersection {:a 1 :b 2 :c 3} {:a 1 :b 2})))
    (is (= {:a 1 :b 2} (intersection {:a 1 :b 2} {:a 1 :b 2 :c 3}))))

  (testing "map values for same key intersects right to left, like merge"
    (is (= {:a 1 :b 2} (intersection {:a 1 :b 1} {:a 1 :b 2}))))

  (testing "maps intersects work recursively"
    (is (= {:a 1 :b {:c 3}} (intersection {:a 1 :b {:c 3 :d 4}}
                                          {:a 1 :b {:c 3}})))

    (is (= {:a 1 :b {}} (intersection {:a 1 :b {}}
                                      {:a 1 :b {}}))))

  (testing "seqs use length of longest, takes values from right to left"
    (is (= [1 2] (intersection [1 2] [1 2])))
    (is (= [1 2 nil] (intersection [1 2 3] [1 2])))
    (is (= [1 2 3] (intersection [1 2] [1 2 3]))))

  (testing "maps in seqs are intersected by position"
    (is (= [{:a 1}
            {:a 2 :b 22}
            nil]
           (intersection [{:a 1}
                          {:a 2 :b 2 :c 3}
                          {:a 3 :b 3}]
                         [{:a 1 :b 1}
                          {:a 2 :b 22}]))))

  (testing "compatibility example"
    (let [expected [{:a 1} {:a 2}]
          actual [{:a 1 :b 1} {:a 2 :c 2}]]
      (is (= expected (intersection expected actual))))))


