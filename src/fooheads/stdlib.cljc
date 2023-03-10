(ns fooheads.stdlib
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
    (into {})))


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
