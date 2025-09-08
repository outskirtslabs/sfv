(ns ol.sfv.api-test
  "tests for all the public api functions"
  (:require [clojure.test :refer [deftest testing is are]]
            [ol.sfv :as sfv]))

(deftest parsing-functions-test
  (testing "parse-item"
    (are [input expected] (= expected (sfv/parse-item input))
      "\"hello\"" (sfv/item (sfv/string "hello"))
      "42"        (sfv/item (sfv/integer 42))
      "?1"        (sfv/item (sfv/bool true))
      "foo"       (sfv/item (sfv/token "foo")))

    ;; Test decimal separately due to precision
    (let [result (sfv/parse-item "3.14")]
      (is (sfv/item? result))
      (is (sfv/decimal? (sfv/item-bare result)))))

  (testing "parse-list and parse-dict"
    (is (sfv/sf-list? (sfv/parse-list "\"hello\", \"world\"")))
    (is (sfv/sf-dict? (sfv/parse-dict "a=1, b=2"))))

  (testing "parse with field-type strings"
    (are [type input] (case type
                        "list" (sfv/sf-list? (sfv/parse type input))
                        "dictionary" (sfv/sf-dict? (sfv/parse type input))
                        "item" (sfv/item? (sfv/parse type input)))
      "list"       "\"a\", \"b\""
      "dictionary" "a=1, b=2"
      "item"       "42")))

(deftest serialization-functions-test
  (testing "serialize item"
    (are [item expected] (= expected (sfv/serialize item))
      (sfv/item (sfv/string "hello")) "\"hello\""
      (sfv/item (sfv/integer 42))     "42"
      (sfv/item (sfv/decimal 3.14))   "3.14"
      (sfv/item (sfv/bool true))      "?1"
      (sfv/item (sfv/token "foo"))    "foo"))

  (testing "serialize list and serialize dict"
    (is (= "\"hello\", \"world\""
           (sfv/serialize (sfv/sf-list [(sfv/item (sfv/string "hello"))
                                        (sfv/item (sfv/string "world"))]))))
    (is (= "a=1, b=2"
           (sfv/serialize (sfv/sf-dict [["a" (sfv/item (sfv/integer 1))]
                                        ["b" (sfv/item (sfv/integer 2))]])))))

  (testing "serialize generic"
    (is (= "42" (sfv/serialize (sfv/item (sfv/integer 42)))))))

(deftest bare-value-constructors-test
  (testing "constructors and predicates"
    (are [constructor predicate value] (and (predicate (constructor value))
                                            (not (sfv/string? (constructor value))))
      sfv/token   sfv/token?   "foo"
      sfv/decimal sfv/decimal? 3.14
      sfv/integer sfv/integer? 42
      sfv/dstring sfv/dstring? "display"
      sfv/bytes   sfv/bytes?   (.getBytes "hello")
      sfv/bool    sfv/bool?    true
      sfv/date    sfv/date?    1634567890)

    (let [s (sfv/string "hello")]
      (is (sfv/string? s))
      (is (not (sfv/token? s))))))

(deftest parameter-functions-test
  (testing "params construction and access"
    (let [ps (sfv/params "a" (sfv/integer 1) "b" (sfv/string "test"))]
      (is (= (sfv/integer 1) (sfv/param-get ps "a")))
      (is (= (sfv/string "test") (sfv/param-get ps "b")))
      (is (nil? (sfv/param-get ps "missing")))
      (is (= #{"a" "b"} (set (sfv/param-keys ps)))))

    (let [empty-ps (sfv/params)]
      (is (empty? (sfv/param-keys empty-ps))))))

(deftest item-functions-test
  (testing "item construction and accessors"
    (let [bare-val (sfv/string "test")
          ps       (sfv/params "a" (sfv/integer 1))
          i        (sfv/item bare-val ps)]
      (is (sfv/item? i))
      (is (= bare-val (sfv/item-bare i)))
      (is (= ps (sfv/item-params i))))

    (let [simple-item (sfv/item (sfv/integer 42))]
      (is (sfv/item? simple-item))
      (is (empty? (sfv/param-keys (sfv/item-params simple-item)))))))

(deftest inner-list-functions-test
  (testing "inner-list construction and accessors"
    (let [items [(sfv/item (sfv/string "a")) (sfv/item (sfv/string "b"))]
          il    (sfv/inner-list items)]
      (is (sfv/inner-list? il))
      (is (= items (sfv/inner-items il)))
      (is (empty? (sfv/param-keys (sfv/inner-params il)))))))

(deftest list-functions-test
  (testing "sf-list construction and accessors"
    (let [members [(sfv/item (sfv/string "hello"))
                   (sfv/inner-list [(sfv/item (sfv/integer 1))])]
          l       (sfv/sf-list members)]
      (is (sfv/sf-list? l))
      (is (= members (sfv/list-members l))))

    (let [empty-list (sfv/sf-list [])]
      (is (sfv/sf-list? empty-list))
      (is (empty? (sfv/list-members empty-list))))))

(deftest dictionary-functions-test
  (testing "sf-dict construction and accessors"
    (let [entries [["key1" (sfv/item (sfv/string "value1"))]
                   ["key2" (sfv/flag)]]
          d       (sfv/sf-dict entries)]
      (is (sfv/sf-dict? d))
      (is (= #{"key1" "key2"} (set (sfv/dict-keys d))))
      (is (= (sfv/item (sfv/string "value1")) (sfv/dict-get d "key1")))
      (is (= (sfv/flag) (sfv/dict-get d "key2")))
      (is (nil? (sfv/dict-get d "missing")))
      (is (= entries (sfv/dict->pairs d))))

    (let [empty-dict (sfv/sf-dict [])]
      (is (sfv/sf-dict? empty-dict))
      (is (empty? (sfv/dict-keys empty-dict))))))

(deftest flag-functions-test
  (testing "flag construction"
    (let [f (sfv/flag)]
      (is (sfv/flag? f))
      (is (not (sfv/item? f)))
      (is (= {:type :flag :params []} f)))

    (let [f-with-params (sfv/flag (sfv/params "a" (sfv/integer 1)))]
      (is (sfv/flag? f-with-params))
      (is (= 1 (count (sfv/param-keys (:params f-with-params))))))))

(deftest roundtrip-test
  (testing "parse/serialize roundtrip"
    (are [item] (= item (sfv/parse-item (sfv/serialize item)))
      (sfv/item (sfv/string "hello"))
      (sfv/item (sfv/integer 42))
      (sfv/item (sfv/bool true))
      (sfv/item (sfv/token "test")))

    (let [list-val (sfv/sf-list [(sfv/item (sfv/string "a"))])]
      (is (= list-val (sfv/parse-list (sfv/serialize list-val)))))

    (let [dict-val (sfv/sf-dict [["a" (sfv/item (sfv/integer 1))]])]
      (is (= dict-val (sfv/parse-dict (sfv/serialize dict-val)))))))
