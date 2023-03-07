(ns fooheads.runtime)


(defn re->str
  "Return the string represenation of a regular expression"
  [re]
  #?(:cljs (let [s (str re)]
             (subs s 1 (dec (count s))))
     :default (str re)))
