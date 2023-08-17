(ns fooheads.stdlib
  (:refer-clojure :exclude [empty into])
  (:require
    #?(:clj [clojure.pprint])
    [clojure.string :as str]
    [fooheads.runtime :as runtime])
  #?(:cljs
     (:require-macros
       [fooheads.stdlib])))


(def ^:private template-param-re #"\{(.*?)\}")
(def ^:private backslash #?(:clj "\\\\" :cljs "\\"))
(def ^:private str-curly-left (str backslash "{"))
(def ^:private str-curly-right (str backslash "}"))


(defn cljs-env?
  "Take the &env from a macro, and tell whether we are expanding into cljs."
  [env]
  (boolean (:js-globals env)))


(defn render-template
  "Renders the template using args for data.
  Example:

  `(render-template \"Error: {error-code}\" {:error-code 11})`
  ; => \"Error: 11\" "
  [template args]
  (->>
    template
    (re-seq template-param-re)
    (reduce
      (fn [template [re k]]
        (let [re (-> re
                     (str/replace #"\{" str-curly-left)
                     (str/replace #"\}" str-curly-right))
              arg (or
                    (get args (keyword k))
                    (get args (symbol k))
                    (get args (str k)))

              arg-str (if (nil? arg) "nil" (str arg))]

          (str/replace template (re-pattern re) arg-str)))
      template)))


(defn template-params
  "Calculate a sequence of params required by the template"
  [template]
  (->>
    template
    (re-seq template-param-re)
    (mapv second)
    (map symbol)))


(defn binding-map
  [symbols]
  (->>
    symbols
    (map #(vector (keyword %) %))
    (clojure.core/into {})))


(defmacro throw-ex
  "Throw an exception with a msg template and optional vars.
  The msg template will be rendered using the scoped vars and
  the template args and vars will be put in the map to ex-info.

  Example:

  `(let [namn \"foohead\"
         age 8]
     (throw-ex \"Hello {namn}\" age))

  ; => {:age 8, :namn \"foohead\", :msg \"Hello foohead\"}`
  "
  [msg-template & symbols]
  (let [symbols (->> msg-template (template-params) (concat symbols))
        arg-map# (binding-map symbols)]
    `(let [msg# (render-template ~msg-template ~arg-map#)
           full-arg-map# (merge ~arg-map# {:msg msg#})]
       (throw (ex-info msg# full-arg-map#)))))


(defmacro dprn
  "Renders the template using the scoped vars,
  println's the msg and pprint's the arg-map"
  [msg-template & symbols]
  (if (cljs-env? &env)
    `(println "dprn not implemented in cljs")
    (let [template-args# (binding-map (template-params msg-template))
          symbol-map# (binding-map symbols)]
      `(let [msg# (render-template ~msg-template ~template-args#)]
         (println msg#)
         (when (seq ~symbol-map#)
           (clojure.pprint/pprint ~symbol-map#))))))


(defn map-keys
  "Apply f to all keys in m"
  [f m]
  (reduce-kv (fn [m k v] (assoc m (f k) v)) {} m))


(defn map-vals
  "Apply f to all values in m"
  [f m]
  (reduce-kv (fn [m k v] (assoc m k (f v))) {} m))


(defn dissoc-vals
  "Dissoc elements from m for which value matches pred"
  [m pred]
  (apply dissoc m (for [[k v] m :when (pred v)] k)))


(defn qualify-ident
  "Qualifies an ident. nspace and nameable must of same
  type. The nspace must be a `simple-ident?` and nameable
  can be either `simple-ident?` or `qualified-ident?`.

  If nameable is already qualified, it will change namespace."
  [nspace nameable]
  (cond
    (and (simple-symbol? nspace) (symbol? nameable))
    (symbol (name nspace) (name nameable))

    (and (simple-keyword? nspace) (keyword? nameable))
    (keyword (name nspace) (name nameable))

    :else
    (let [msg (str "Unsupported combination: " (or nspace "nil")
                   " " (or nameable "nil"))]
      (throw (ex-info
               msg {:msg msg :nspace nspace :nameable nameable})))))


(defn unqualify-ident [ident]
  (cond
    (symbol? ident)
    (symbol (name ident))

    (keyword? ident)
    (keyword (name ident))

    :else
    (let [msg (str "Can't unqualify " (or ident "nil"))]
      (throw (ex-info msg {:msg msg :ident ident})))))


(defn qualify-keys
  "Qualify all the keys in a map"
  [m nspace]
  (map-keys (partial qualify-ident nspace) m))


(defn qualified-name
  "Returns a string with the qualified name of the ident"
  [ident]
  (cond
    (nil? ident)
    (let [msg "Not an ident: nil"]
      (throw (ex-info msg {:msg msg :ident ident})))

    (qualified-ident? ident)
    (str (namespace ident) "/" (name ident))

    :else
    (name ident)))


(defn named?
  "Returns true if `clojure.core/name` can be called on x."
  [x]
  (or (string? x) (ident? x)))


(defmacro guard
  "A guard that throws an exception (with msg) if v does not meet pred."
  ([pred v msg]
   `(let [v# ~v
          pred# ~pred
          msg# ~msg]
      (if (pred# v#)
        v#
        (throw (ex-info msg# {:guard/msg msg#
                              :guard/pred pred#
                              :guard/value v#})))))
  ([pred v msg data-map]
   `(let [v# ~v
          pred# ~pred
          msg# ~msg
          data-map# ~data-map]
      (if (pred# v#)
        v#
        (throw (ex-info msg# (merge data-map# {:guard/msg msg#
                                               :guard/pred pred#
                                               :guard/value v#})))))))


(defn apply-if
  "Returns a function that applies f if pred returns true for the specified
  value.

  Usually used in combination with `clojure.walk/postwalk`, e.g.

  `(postwalk (apply-if keyword? str) coll)`
  "
  [pred f & args]
  (fn [v]
    (if (pred v)
      (apply f (cons v args))
      v)))


(defn simple-keyword
  "Forces a simple keyword. Useful when forward slashes can
  be present in the string `s`"
  [s]
  (keyword nil s))


(defn simple-symbol
  "Forces a simple symbol Useful when forward slashes can
  be present in the string `s`"
  [s]
  (symbol nil s))


(defn simple-ident
  "Takes an ident and unqualifies it (removed the namespace). Returns the same
  type of ident as was provided. Returns nil if `ident` is not an ident."
  [ident]
  (cond
    (keyword? ident) (keyword (name ident))
    (symbol? ident)  (symbol (name ident))
    :else nil))


(def ^:private example-regex #"")


(defn regex?
  "Test whether x is a regular expression"
  [x]
  (= (type x) (type example-regex)))


(defn substring [s start end]
  (if (= -1 end)
    (subs s start (dec (count s)))
    (subs s start end)))


(def re->str runtime/re->str)


(defn into
  "Like clojure.core/into, but keeps the original order for lists.
  Does not support transducers."
  ([]
   (into))
  ([to]
   (into to))
  ([to from]
   (let [from (if (list? to) (reverse from) from)]
     (clojure.core/into to from))))


(defn empty
  "Like clojure.core/empty with support for map-entry."
  [coll]
  (if (map-entry? coll)
    []
    (clojure.core/empty coll)))


(defn exceptional
  "Returns a function that applies f and uses the specified
  get function to retrieve the value if success? returns true for the
  function application.

  Throws an exception if success? returns something falsy."
  [f success? get]
  (fn [& args]
    (let [res (apply f args)]
      (if (success? res)
        (get res)
        (throw (ex-info "Exceptional failure" {:failure res}))))))


(defn exactly=
  "Checks that `=` is true and that both x and y are of the same type."
  [x y]
  (and
    (= x y)
    (= (type x) (type y))))


(defn seqt
  "Like `seq` but returns the original coll if not empty"
  [coll]
  (if (seq coll) coll nil))


(defn const
  "Like `cons` but keeps the collection type."
  [x coll]
  (into (empty coll) (cons x coll)))


(defn conjt
  "Like `conj`, keeps the collection type and throws error for `seq?` collections,
  since it would be very slow (and in the \"wrong\" order). If coll is nil,
  defaults to vector (instead of list as conj does)."
  [coll x]
  (cond
    (nil? coll)
    (conjt [] x)

    (seq? coll)
    (let [msg "Can't conjt on `seq?` collections."]
      (throw (ex-info msg {:msg msg :coll coll :x x})))

    :else
    (conj coll x)))


(defn cons-some
  "Like `cons` but does not add x if x is nil."
  [x coll]
  (if x (cons x coll) (seq coll)))


(defn conj-some
  "Like `conj` but does not add x if x is nil."
  [coll x]
  (if x (conj coll x) coll))


(defn const-some
  "Like `const` but does not add x if x is nil."
  [x coll]
  (if x (const x coll) coll))


(defn conjt-some
  "Like `conjt` but does not add x if x is nil."
  [coll x]
  (cond
    (nil? coll)
    (conjt-some [] x)

    (some? x)
    (conjt coll x)

    :else
    coll))


(defn removet
  "Like `remove` but keeps the collection type."
  [pred coll]
  (into (empty coll) (remove pred coll)))


(defn filtert
  "Like `filter` but keeps the collection type."
  [pred coll]
  (into (empty coll) (filter pred coll)))


(defn mapt
  "Like `map` but keeps the collection type. For maps, it assumes that
  f returns a pair/map-entry, in order to successfully turn the values
  into a map again. When multiple collections are provided, the type of
  the first collection determines the return type."
  ([f coll]
   (into (empty coll) (map f coll)))
  ([f c1 c2]
   (into (empty c1) (map f c1 c2)))
  ([f c1 c2 c3]
   (into (empty c1) (map f c1 c2 c3)))
  ([f c1 c2 c3 & colls]
   (into (empty c1) (apply map f c1 c2 c3 colls))))


(defn transpose
  "Transpose a seq of seqs. Returns a vector of vectors."
  [xs]
  (if (seq xs)
    (apply mapv vector xs)
    xs))


(defn singleton?
  "Returns true if the `coll` has exactly one element, false otherwise."
  [coll]
  (= 1 (count coll)))


(defn partition-using
  "Collects values to a partition until the pred is true for the partition,
  and then starts a new partition.  If pred never is true, all values ends
  up in a single partition.  Returns a lazy seq of partitions.

  When I grow up, I want to return a (stateful) transducer when only pred is
  provided."
  ([pred coll]
   (lazy-seq
     (when-let [s (seq coll)]
       (loop [s s
              run []]
         (if (empty? s)
           [run]
           (let [run (conj run (first s))
                 s (lazy-seq (rest s))]
             (if (pred run)
               (cons run (partition-using pred s))
               (recur s run)))))))))

