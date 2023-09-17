(ns fooheads.test
  (:require
    [clojure.test :refer [is]]
    [fooheads.shape :as shape]
    [fooheads.stdlib :refer [cljs-env?]]
    [lambdaisland.deep-diff2 :as ddiff])
  #?(:cljs
     (:require-macros
       [fooheads.test])))


(defn ex-symbol
  [env]
  (if (cljs-env? env) 'js/Error 'Exception))


(defmacro thrown-ex-data
  "Catches an exception in `body` and extracts the ex-data, to make it easy to
  test exceptional code. If `ks` is given, these keys are extracted from the
  ex-data map.

  If an exception is not thrown, it returns
  `{:expected-exception :was-not-thrown}`"
  ([body]
   `(thrown-ex-data nil ~body))
  ([ks body]
   `(try
      ~body
      (throw (ex-info "Exception was not thrown" {:expected-exception :was-not-thrown}))
      (catch ~(ex-symbol &env) e#
        (let [ks# ~ks
              data# (ex-data e#)]
          (cond
            (contains? data# :expected-exception)
            data#

            (nil? ks#)
            data#

            :else
            (select-keys data# ks#)))))))


(defmacro should-be
  [expected actual]
  `(let [actual# ~actual
         expected# ~expected
         actual'# (shape/intersection expected# actual#)]

     (when-not (is (= expected# actual'#))
       (ddiff/pretty-print (ddiff/diff expected# actual'#)))))


