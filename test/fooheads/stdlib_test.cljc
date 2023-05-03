(ns fooheads.stdlib-test
  (:require
    [clojure.test :refer [are deftest is]]
    [fooheads.stdlib :as std :refer [dprn
                                     dissoc-vals
                                     map-vals map-keys
                                     qualified-name
                                     qualify-ident
                                     re->str
                                     regex?
                                     render-template template-params
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

