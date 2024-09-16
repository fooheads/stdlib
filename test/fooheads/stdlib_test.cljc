(ns fooheads.stdlib-test
  (:require
    [clojure.test :refer [are deftest is testing]]
    [fooheads.stdlib :as std :refer [conj-some
                                     conjt
                                     conjt-some
                                     cons-some
                                     const-some
                                     const
                                     dprn
                                     dissoc-vals
                                     every-partition?
                                     exactly=
                                     exceptional
                                     filtert
                                     filter-indexes
                                     forv
                                     index-of
                                     index-of-all
                                     map-vals
                                     map-keys
                                     mapt
                                     named?
                                     partition-using
                                     partition-indexes
                                     qualified-name
                                     qualify-ident
                                     re->str
                                     regex?
                                     removet
                                     remove-indexes
                                     render-template template-params
                                     seq->
                                     seq->>
                                     seqt
                                     simple-ident
                                     simple-keyword
                                     singleton?
                                     simple-symbol
                                     some-partition
                                     substring
                                     throw-ex
                                     transpose
                                     unqualify-ident]
     :include-macros true]
    [fooheads.test :include-macros true]))


(deftest render-template-test
  (is (= "Error: 28 and Jimi and :guitarist and nil"
         (render-template "Error: {age} and {name} and {type} and {wife}"
                          {'age 28 :name "Jimi" "type" :guitarist}))))


(deftest template-params-test
  (is (= ['age 'name 'type 'wife]
         (template-params "Error: {age} and {name} and {type} and {wife}"))))


#_{:clj-kondo/ignore [:unused-binding]}


(deftest throw-ex-test
  (is (= {:age 8, :namn "foohead", :msg "Hello foohead"}
         (fooheads.test/thrown-ex-data
           (let [namn "foohead"
                 age 8]
             (throw-ex "Hello {namn}" age))))))


#_{:clj-kondo/ignore [:unused-binding]}


(deftest dprn-test
  (is (= #?(:cljs "dprn not implemented in cljs\n"
            :default "Hello foohead\n")
         (with-out-str
           (let [namn "foohead"]
             (dprn "Hello {namn}")))))

  (is (= #?(:cljs "dprn not implemented in cljs\n"
            :default "Hello foohead\n{:age 8}\n")
         (with-out-str
           (let [namn "foohead"
                 age 8]
             (dprn "Hello {namn}" age))))))


(deftest map-vals-test
  (is (= {:a 2 :b 3 :c 4}
         (map-vals inc {:a 1 :b 2 :c 3})))

  (is (= {:a :foo :b :bar}
         (map-vals keyword {:a "foo" :b "bar"}))))


(deftest map-keys-test
  (is (= {2 :a 3 :b}
         (map-keys inc {1 :a 2 :b})))

  (is (= {"1" :a "2" :b}
         (map-keys str {1 :a 2 :b})))

  (is (= {:a 1 :b 2}
         (map-keys keyword {"a" 1 "b" 2}))))


(deftest dissoc-vals-test
  (is (= {:a 1} (dissoc-vals {:a 1 :b nil} nil?)))
  (is (= {:a 1} (dissoc-vals {:a 1 :b 2} even?)))
  (is (nil? (dissoc-vals nil even?))))


(deftest qualify-ident-test
  (are [expected nspace nameable]
       (= expected (qualify-ident nspace nameable))

    :foo/bar :foo :bar
    'foo/bar 'foo 'bar

    :new-foo/bar :new-foo :foo/bar
    'new-foo/bar 'new-foo 'foo/bar)

  (is (= {:msg "Unsupported combination: nil :bar" :nspace nil :nameable :bar}
         (fooheads.test/thrown-ex-data
           (qualify-ident nil :bar)))))


(deftest unqualify-ident-test
  (are [expected actual] (= expected actual)

    :bar (unqualify-ident :foo/bar)
    'bar (unqualify-ident 'foo/bar))

  (is (= {:msg "Can't unqualify 1" :ident 1}
         (fooheads.test/thrown-ex-data (unqualify-ident 1))))

  (is (= {:msg "Can't unqualify nil" :ident nil}
         (fooheads.test/thrown-ex-data (unqualify-ident nil)))))


(deftest qualified-name-test
  (are [expected ident] (= expected (qualified-name ident))

    "bar"     :bar
    "bar"     'bar

    "foo/bar" :foo/bar
    "foo/bar" 'foo/bar)

  (is (= {:msg "Not an ident: nil" :ident nil}
         (fooheads.test/thrown-ex-data
           (qualified-name nil)))))


(deftest named?-test
  (are [expected actual] (exactly= expected actual)
    false (named? nil)
    false (named? [])
    false (named? 1)
    true  (named? "foo")
    true  (named? :foo)
    true  (named? 'foo)
    true  (named? :ns/foo)
    true  (named? 'ns/foo)))


(deftest simple-keyword-test
  (is (simple-keyword? (simple-keyword "foo")))
  (is (= :foo (simple-keyword "foo")))

  (is (simple-keyword? (simple-keyword "foo/bar")))
  (is (= (keyword nil "foo/bar") (simple-keyword "foo/bar"))))


(deftest simple-symbol-test
  (is (simple-symbol? (simple-symbol "foo")))
  (is (= 'foo (simple-symbol "foo")))

  (is (simple-symbol? (simple-symbol "foo/bar")))
  (is (= (symbol nil "foo/bar") (simple-symbol "foo/bar"))))


(deftest simple-ident-test
  (is (= :bar (simple-ident :bar)))
  (is (= :bar (simple-ident :foo/bar)))
  (is (= 'bar (simple-ident 'bar)))
  (is (= 'bar (simple-ident 'foo/bar)))
  (is (nil? (simple-ident nil)))
  (is (nil? (simple-ident "foo/bar"))))


(deftest regex?-test
  (is (true? (regex? #"foo"))))


(deftest substring-test
  (is (= "o" (substring "foo" 1 -1))))


(deftest re->str-test
  (is (= "[0-9]" (re->str #"[0-9]"))))


(deftest into-test
  (is (= '(1 2 3)    (std/into '() '(1 2 3))))
  (is (= '(1 2 3)    (std/into '() [1 2 3])))
  (is (= [1 2 3]     (std/into [] [1 2 3])))
  (is (= {:a 1 :b 2} (std/into {} {:a 1 :b 2})))
  (is (= {:a 1 :b 2} (std/into {} [[:a 1] [:b 2]]))))


(deftest empty-test
  (is (= [] (std/empty (first {:a 1}))))
  (is (= '() (std/empty (list 1 2)))))


(deftest exceptional-test
  (let [success? (fn [m] (contains? m :success))
        get-value (fn [m] (:success m))]

    (testing 'success
      (let [f (fn [x] {:success x})
            g (exceptional f success? get-value)]
        (is (= :foo (g :foo)))))

    (testing 'failure
      (let [f (fn [x] {:error x})
            g (exceptional f success? get-value)]
        (is (= {:failure {:error :foo}}
               (fooheads.test/thrown-ex-data
                 (g :foo))))))))


(deftest exactly=-test
  (is (exactly= [] []))
  (is (exactly= [1] [1]))
  (is (not (exactly= '() [])))
  (is (not (exactly= [1] [1 2]))))


(deftest seqt-test
  (are [expected actual] (exactly= expected actual)
    nil         (seqt nil)
    nil         (seqt [])
    nil         (seqt (list))
    nil         (seqt #{})
    nil         (seqt (range 1 1))

    '(1)        (seqt (list 1))
    [1]         (seqt [1])
    #{1}        (seqt #{1})
    {:a 1}      (seqt {:a 1})
    (range 1 3) (seqt (range 1 3))))


(deftest const-test
  (are [expected actual] (exactly= expected actual)
    '(nil)      (const nil nil)
    '(1)        (const 1 nil)
    '(1 2 3)    (const 1 '(2 3))
    [1 2 3]     (const 1 [2 3])
    #{1 2 3}    (const 1 #{2 3})
    {:a 1 :b 2} (const [:b 2] {:a 1})
    '(1 2 3)    (const 1 (range 2 4))))


(deftest conjt-test
  (are [expected actual] (exactly= expected actual)
    [nil]       (conjt nil nil)
    [1]         (conjt nil 1)
    [1 2 3]     (conjt [1 2] 3)
    #{1 2 3}    (conjt #{1 2} 3)
    {:a 1 :b 2} (conjt {:a 1} [:b 2]))

  (is (= {:coll '(2 3) :x 1
          :msg "Can't conjt on `seq?` collections."}
         (fooheads.test/thrown-ex-data
           (conjt '(2 3) 1)))))


(deftest cons-some-test
  (are [expected actual] (= expected actual)
    nil      (cons-some nil nil)
    '(1 2)   (cons-some nil [1 2])
    '(1)     (cons-some 1 nil)
    '(1 2 3) (cons-some 1 [2 3])))


(deftest conj-some-test
  (are [expected actual] (exactly= expected actual)
    nil      (conj-some nil nil)
    [1 2]    (conj-some [1 2] nil)
    '(1)     (conj-some nil 1)
    [1 2 3]  (conj-some [1 2] 3)))


(deftest const-some-test
  (are [expected actual] (exactly= expected actual)
    nil     (const-some nil nil)
    [1 2]   (const-some nil [1 2])
    '(1)    (const-some 1 nil)
    [1 2 3] (const-some 1 [2 3])))


(deftest conjt-some-test
  (are [expected actual] (exactly= expected actual)
    []      (conjt-some nil nil)
    [1 2]   (conjt-some [1 2] nil)
    [1]     (conjt-some nil 1)
    [1 2 3] (conjt-some [1 2] 3)))


(deftest removet-test
  (are [expected actual] (exactly= expected actual)
    nil    (removet even? nil)
    '(1 3) (removet even? '(1 2 3))
    [1 3]  (removet even? [1 2 3])
    #{1 3} (removet even? #{1 2 3})
    {:b 2} (removet #(= [:a 1] %) {:a 1 :b 2})))


(deftest filtert-test
  (are [expected actual] (exactly= expected actual)
    nil    (filtert odd? nil)
    '(1 3) (filtert odd? '(1 2 3))
    [1 3]  (filtert odd? [1 2 3])
    #{1 3} (filtert odd? #{1 2 3})
    {:b 2} (filtert #(= [:b 2] %) {:a 1 :b 2})))


(deftest mapt-test
  (are [expected actual] (exactly= expected actual)
    nil         (mapt inc nil)
    '(2 3)      (mapt inc '(1 2))
    [2 3]       (mapt inc [1 2])
    #{2 3}      (mapt inc #{1 2})
    {:a 2 :b 3} (mapt (fn [[k v]] [k (inc v)]) {:a 1 :b 2})

    [5 7 9]     (mapt + [1 2 3] [4 5 6])
    '(12 15 18) (mapt + '(1 2 3) [4 5 6] [7 8 9])
    #{6}        (mapt + #{1} [2] '(3))

    [5 10]      (mapt + [1 2] [1 2] [1 2] [1 2] [1 2])))


(deftest transpose-test
  (are [expected actual] (exactly= expected actual)
    nil                        (transpose nil)
    []                         (transpose [])
    []                         (transpose [nil nil])
    []                         (transpose [[] []])
    [[:a 1] [:b 2]]            (transpose [[:a :b] [1 2]])
    [[:a 1] [:b 2]]            (transpose '((:a :b) (1 2)))
    [[\a \1] [\b \2] [\c \3]]  (transpose ["abc" "123"])))


(deftest singleton?-test
  (are [expected actual] (exactly= expected actual)
    false  (singleton? nil)
    false  (singleton? [])
    true   (singleton? [1])
    false  (singleton? [1 2])))


;; TODO: Are these two pred generator function be in fooheads.core?
;; Perhaps there is a better name for the functions?

(defn- num-eq [n v]
  (fn [coll]
    (= n (count (filter #(= % v) coll)))))


(defn- size [n]
  (fn [coll]
    (= n (count coll))))


(deftest partition-using-test
  (is (= []
         (partition-using any? nil)))

  (is (= []
         (partition-using any? [])))

  (is (= [[1]]
         (partition-using any? [1])))

  (is (= [[1] [2] [3]]
         (partition-using any? [1 2 3])))

  (is (= [[1 2 3]]
         (partition-using (size 10) [1 2 3])))

  (is (= [[1 2] [3 4] [5]]
         (partition-using (size 2) [1 2 3 4 5])))

  (is (= '[[| 1 | 2 | 3 |]]
         (partition-using (num-eq 4 '|)
                          '[| 1 | 2 | 3 |])))

  (is (= '[[| 1 | 2 | 3 |]
           [| 4 | 5 | 6 |]]
         (partition-using (num-eq 4 '|)
                          '[| 1 | 2 | 3 |   | 4 | 5 | 6 |])))

  (is (= '[[| 1 | 2 | 3 |]
           [| 4 | 5 | 6 |]
           [| 7 | 8 | 9 |]]
         (partition-using (num-eq 4 '|)
                          '[| 1 | 2 | 3 |
                            | 4 | 5 | 6 |
                            | 7 | 8 | 9 |]))))


(deftest partition-indexes-test
  (are [expected actual] (= expected actual)
    []                     (partition-indexes [] [])
    [[:a]]                 (partition-indexes [] [:a])
    [[:a] []]              (partition-indexes [[0]] [:a])
    [[] [:a]]              (partition-indexes [[]] [:a])
    [[:a :d] [:b] [:c :e]] (partition-indexes [[0 3] [1]] [:a :b :c :d :e])
    [[:a :d] [:b] [:c :e]] (partition-indexes ['(0 3) [1]] [:a :b :c :d :e])))


(deftest filter-indexes-test
  (are [expected actual] (= expected actual)
    []         (filter-indexes [] [:a :b :c])
    [:a]       (filter-indexes [0] [:a :b :c])
    [:a :b]    (filter-indexes [0 1] [:a :b :c])
    [:b :c]    (filter-indexes [1 2] [:a :b :c])
    [:a :b :c] (filter-indexes [0 1 2] [:a :b :c])
    [:a :b :c] (filter-indexes [0 1 2 3] [:a :b :c])))


(deftest remove-indexes-test
  (are [expected actual] (= expected actual)
    [:a :b :c] (remove-indexes [] [:a :b :c])
    [:b :c]    (remove-indexes [0] [:a :b :c])
    [:c]       (remove-indexes [0 1] [:a :b :c])
    [:a]       (remove-indexes [1 2] [:a :b :c])
    []         (remove-indexes [0 1 2] [:a :b :c])
    []         (remove-indexes [0 1 2 3] [:a :b :c])))


(deftest index-of-test
  (are [expected actual] (= expected actual)
    nil (index-of #{9} [1 2 3])
    1 (index-of #{2} [1 2 3])
    1 (index-of even? [1 2 3])))


(deftest index-of-all-test
  (are [expected actual] (= expected actual)
    [1]   (index-of-all even? [1 2 3])
    [0 2] (index-of-all odd? [1 2 3])
    []    (index-of-all odd? [2 4 6])))


(deftest seq->-test
  (is (nil?  (seq-> nil)))
  (is (nil?  (seq-> {})))
  (is (nil?  (seq-> {:a 1} (dissoc :a) (vals))))
  (is (= [2] (seq-> {:a 1 :b 2} (dissoc :a) (vals)))))


(deftest seq->>-test
  (is (nil? (seq->> nil)))
  (is (nil? (seq->> [])))
  (is (nil? (seq->> [2 4 6] (filter odd?) (apply +))))
  (is (= 4  (seq->> [1 2 3] (filter odd?) (apply +)))))


(deftest forv-test
  (is (= [2 4]
         (forv [i (range 1 6)
                :when (even? i)]
               i)))

  (is (vector?
        (forv [i (range 1 6)
               :when (even? i)]
              i))))


(deftest some-partition-test
  (is (nil? (some-partition > 2 [1 2 3 4 5 6])))
  (is (= [4 3] (some-partition > 2 [1 2 4 3 5 6]))))


(deftest every-partition?-test
  (is (true? (every-partition? < 2 [1 2 3 4 5 6])))
  (is (false? (every-partition? < 2 [1 2 4 3 5 6]))))

