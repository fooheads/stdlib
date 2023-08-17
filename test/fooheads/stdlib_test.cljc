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
                                     exactly=
                                     exceptional
                                     map-vals map-keys
                                     qualified-name
                                     qualify-ident
                                     re->str
                                     regex?
                                     render-template template-params
                                     seqt
                                     simple-keyword
                                     substring
                                     throw-ex]
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


(deftest qualified-name-test
  (are [expected ident] (= expected (qualified-name ident))

    "bar"     :bar
    "bar"     'bar

    "foo/bar" :foo/bar
    "foo/bar" 'foo/bar)

  (is (= {:msg "Not an ident: nil" :ident nil}
         (fooheads.test/thrown-ex-data
           (qualified-name nil)))))


(deftest simple-keyword-test
  (is (simple-keyword? (simple-keyword "foo")))
  (is (simple-keyword? (simple-keyword "foo/bar"))))


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

