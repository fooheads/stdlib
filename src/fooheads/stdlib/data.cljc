(ns fooheads.stdlib.data)


(defn hiccup->clojure-xml
  "Converts hiccup into clojure.xml format."
  [hiccup]
  (cond
    (string? hiccup)
    hiccup

    (vector? hiccup)
    (if (and (seq hiccup) (every? vector? hiccup))
      (map hiccup->clojure-xml hiccup)
      (let [[tag & body] hiccup
            [attrs content] (if (map? (first body))
                              [(first body) (rest body)]
                              [{} body])]
        {:tag tag
         :attrs attrs
         :content (map hiccup->clojure-xml content)}))

    (seq? hiccup)
    (map hiccup->clojure-xml hiccup)

    :else
    hiccup))


(defn clojure-xml->hiccup
  "Converts clojure.xml format into hiccup."
  [xml]
  (cond
    (string? xml)
    xml

    (map? xml)
    (let [{:keys [tag attrs content]} xml
          hiccup-content (map clojure-xml->hiccup content)]
      (vec (cons tag (if (empty? attrs)
                       hiccup-content
                       (cons attrs hiccup-content)))))

    (sequential? xml)
    (mapv clojure-xml->hiccup xml)

    :else xml))

