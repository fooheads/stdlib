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
  [body]
  `(try
     ~body
     (throw (ex-info "Exception was not thrown" {:expected-exception :was-not-thrown}))
     (catch ~(ex-symbol &env) e#
       (ex-data e#))))


(defmacro should-be
  [expected actual]
  `(let [actual# ~actual
         expected# ~expected
         actual'# (shape/intersection expected# actual#)]

     (when-not (is (= expected# actual'#))
       (ddiff/pretty-print (ddiff/diff expected# actual'#)))))


