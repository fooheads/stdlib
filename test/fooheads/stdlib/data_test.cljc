(ns fooheads.stdlib.data-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [fooheads.stdlib.data :as data]))


(deftest hiccup->clojure-xml-test
  (testing "string input"
    (is (= "hello" (data/hiccup->clojure-xml "hello"))))

  (testing "simple tag without attributes"
    (is (= {:tag :div
            :attrs {}
            :content ["hello"]}
           (data/hiccup->clojure-xml [:div "hello"]))))

  (testing "tag with attributes"
    (is (= {:tag :div
            :attrs {:class "container"}
            :content ["hello"]}
           (data/hiccup->clojure-xml [:div {:class "container"} "hello"]))))

  (testing "nested tags"
    (is (= {:tag :div
            :attrs {}
            :content [{:tag :p
                       :attrs {}
                       :content ["hello"]}]}
           (data/hiccup->clojure-xml [:div [:p "hello"]]))))

  (testing "multiple children"
    (is (= {:tag :div
            :attrs {}
            :content [{:tag :p
                       :attrs {}
                       :content ["first"]}
                      {:tag :p
                       :attrs {}
                       :content ["second"]}]}
           (data/hiccup->clojure-xml [:div [:p "first"] [:p "second"]]))))

  (testing "sequence input"
    (is (= [{:tag :p
             :attrs {}
             :content ["first"]}
            {:tag :p
             :attrs {}
             :content ["second"]}]
           (data/hiccup->clojure-xml [[:p "first"] [:p "second"]]))))

  (testing "non-string/vector/seq input"
    (is (= 42 (data/hiccup->clojure-xml 42)))))


(deftest clojure-xml->hiccup-test
  (testing "string input"
    (is (= "hello"
           (data/clojure-xml->hiccup "hello"))))

  (testing "simple tag without attributes"
    (is (= [:div "hello"]
           (data/clojure-xml->hiccup
            {:tag :div
             :attrs {}
             :content ["hello"]}))))

  (testing "tag with attributes"
    (is (= [:div {:class "container"} "hello"]
           (data/clojure-xml->hiccup
            {:tag :div
             :attrs {:class "container"}
             :content ["hello"]}))))

  (testing "nested tags"
    (is (= [:div [:p "hello"]]
           (data/clojure-xml->hiccup
            {:tag :div
             :attrs {}
             :content [{:tag :p
                        :attrs {}
                        :content ["hello"]}]}))))

  (testing "multiple children"
    (is (= [:div [:p "first"] [:p "second"]]
           (data/clojure-xml->hiccup
            {:tag :div
             :attrs {}
             :content [{:tag :p
                        :attrs {}
                        :content ["first"]}
                       {:tag :p
                        :attrs {}
                        :content ["second"]}]}))))

  (testing "sequence input"
    (is (= [[:p "first"] [:p "second"]]
           (data/clojure-xml->hiccup
            [{:tag :p
              :attrs {}
              :content ["first"]}
             {:tag :p
              :attrs {}
              :content ["second"]}]))))

  (testing "non-string/map/seq input"
    (is (= 42
           (data/clojure-xml->hiccup 42)))))


(deftest roundtrip-test
  (testing "hiccup->clojure-xml->hiccup roundtrip"
    (let [hiccup [:div {:class "container"}
                  [:h1 "Title"]
                  [:p "Paragraph"]
                  [:ul
                   [:li "Item 1"]
                   [:li "Item 2"]]]]
      (is (= hiccup (-> hiccup
                        data/hiccup->clojure-xml
                        data/clojure-xml->hiccup)))))

  (testing "clojure-xml->hiccup->clojure-xml roundtrip"
    (let [xml {:tag :div
               :attrs {:class "container"}
               :content [{:tag :h1
                          :attrs {}
                          :content ["Title"]}
                         {:tag :p
                          :attrs {}
                          :content ["Paragraph"]}
                         {:tag :ul
                          :attrs {}
                          :content [{:tag :li
                                     :attrs {}
                                     :content ["Item 1"]}
                                    {:tag :li
                                     :attrs {}
                                     :content ["Item 2"]}]}]}]
      (is (= xml (-> xml
                     data/clojure-xml->hiccup
                     data/hiccup->clojure-xml))))))

