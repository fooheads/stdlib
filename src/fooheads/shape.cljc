(ns fooheads.shape
  (:require
    [clojure.set :as set]))


(defn intersection
  "Returns the intersection of two shapes"
  [stree tree]
  (cond
    (and (map? stree) (map? tree))
    (let [ks (set/intersection (set (keys stree)) (set (keys tree)))]
      (reduce (fn [m k]
                (assoc m k (intersection (get stree k) (get tree k))))
              {} ks))

    (or
      (and (sequential? stree) (sequential? tree))
      (and (set? stree) (set? tree)))
    (let [n (max (count tree) (count stree))
          streev (vec stree)
          treev (vec tree)]

      (->>
        (range n)
        (map
          (fn [i]
            (intersection (get streev i (get treev i nil)) (get treev i nil))))
        (into (empty stree))))

    :else tree))


