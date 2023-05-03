(ns fooheads.numeral
  (:require
    [fooheads.stdlib :refer [guard]]
    [clojure.math :as math]
    [clojure.string :as str]))


(def ^:private default-digits
  "The default numeral digits (0-9 and A-Z)"
  (->>
    (concat (range 48 58) (range 65 91))
    (map char)
    (vec)))


(def ^:private default-digit-value
  (into {}
    (map-indexed
      (fn [n digit]
        [digit n])
      default-digits)))


(defn digit
  "Returns a digit (0-9, A-Z) for a value. Max base is 36. Returns nil for
  values outside of range."
  [n]
  (get default-digits n))


(defn digit-value
  "Returns the value for a digit (0-9, A-Z). Max base is 36. Returns nil for
  values outside of range."
  [digit]
  (get default-digit-value digit))


(defn digits
  "Returns a seq of digits for a (positive) decimal int. Arguments are:

   n - the decimal representation of the digit, for example
       0 for \\0, 16 for \\F

   base - the numeric base, defaults to 10.

   min-digits - the minimum number of digits needed, defaults to 1.

   digit-fn - a function that takes an int and returns a digit. The default
              implementation is the `digit` function in this namespace.
              If you have the need for other digits, you can provide your own
              function, which doesn not have to returns chars. If you provide
              the identity function, you will get back the numerical value for
              each digit."

  ([n]
   (digits n 10 1 digit))

  ([n base]
   (digits n base 1 digit))

  ([n base min-digits]
   (digits n base min-digits digit))

  ([n base min-digits digit-fn]
   (guard (complement neg?) n "n must be a positive number")
   (guard pos? base "base must be a positive number")
   (guard pos? min-digits "min-digits must be a positive number")
   (guard ifn? digit-fn "digit-fn must be an IFn")
   (loop [pos min-digits
          n n
          stored-digits []]
     (if (or (< 0 n) (< 0 pos))
       (recur (dec pos) (quot n base) (cons (digit-fn (mod n base)) stored-digits))
       stored-digits))))


(defn digit-string
  "Returns a string with the digits for n by calling `digits` and doing a
  clojure.string/join on the resulting digits. See `digits` for more details about
  the arguments."
  ([n]
   (digit-string n 10 1))
  ([n base]
   (digit-string n base 1))
  ([n base min-digits]
   (str/join (digits n base min-digits digit)))
  ([n base min-digits digit-fn]
   (str/join (digits n base min-digits digit-fn))))


(defn value
  "Returns the value for a string or seq of digits.
  base defaults to 10 and digit-value-fn defaults to `digit-value`"
  ([digits]
   (value digits 10))

  ([digits base]
   (value digits base digit-value))

  ([digits base digit-value-fn]
   (if (string? digits)
     (value (seq digits) base digit-value-fn)
     (apply
       +
       (map-indexed
         (fn [index digit]
           (* (int (math/pow base index)) (digit-value-fn digit)))
         (reverse digits))))))

