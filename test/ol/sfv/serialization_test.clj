;; Copyright © 2025 Casey Link <casey@outskirtslabs.com>
;; SPDX-License-Identifier: MIT
(ns ol.sfv.serialization-test
  (:require [clojure.test :refer [deftest is testing]]
            [ol.sfv :as sfv]))

(deftest serialize-bare-items-test
  (testing "Integer serialization"
    (is (= "42" (sfv/serialize (sfv/item (sfv/integer 42)))))
    (is (= "-42" (sfv/serialize (sfv/item (sfv/integer -42)))))
    (is (= "0" (sfv/serialize (sfv/item (sfv/integer 0))))))

  (testing "Decimal serialization"
    (is (= "3.14" (sfv/serialize (sfv/item (sfv/decimal 3.14)))))
    (is (= "-3.14" (sfv/serialize (sfv/item (sfv/decimal -3.14)))))
    (is (= "3.0" (sfv/serialize (sfv/item (sfv/decimal 3.0))))))

  (testing "String serialization"
    (is (= "\"hello\"" (sfv/serialize (sfv/item (sfv/string "hello")))))
    (is (= "\"hello world\"" (sfv/serialize (sfv/item (sfv/string "hello world")))))
    (is (= "\"\\\"quoted\\\"\"" (sfv/serialize (sfv/item (sfv/string "\"quoted\"")))))
    (is (= "\"back\\\\slash\"" (sfv/serialize (sfv/item (sfv/string "back\\slash"))))))

  (testing "Token serialization"
    (is (= "foo" (sfv/serialize (sfv/item (sfv/token "foo")))))
    (is (= "*foo" (sfv/serialize (sfv/item (sfv/token "*foo")))))
    (is (= "foo:bar/baz" (sfv/serialize (sfv/item (sfv/token "foo:bar/baz"))))))

  (testing "Boolean serialization"
    (is (= "?1" (sfv/serialize (sfv/item (sfv/bool true)))))
    (is (= "?0" (sfv/serialize (sfv/item (sfv/bool false))))))

  (testing "Date serialization"
    (is (= "@1659578233" (sfv/serialize (sfv/item (sfv/date 1659578233))))))

  (testing "Byte sequence serialization"
    (is (= ":aGVsbG8=:"
           (sfv/serialize (sfv/item (sfv/bytes (.getBytes "hello"))))))))

(deftest serialize-parameters-test
  (testing "Item with parameters"
    (is (= "42;a=1"
           (sfv/serialize (sfv/item (sfv/integer 42)
                                    (sfv/params "a" (sfv/integer 1))))))
    (is (= "42;a;b=?0"
           (sfv/serialize (sfv/item (sfv/integer 42)
                                    (sfv/params "a" (sfv/bool true)
                                                "b" (sfv/bool false))))))))

(deftest serialize-list-test
  (testing "Simple list"
    (is (= "1, 2, 3"
           (sfv/serialize (sfv/sf-list [(sfv/item (sfv/integer 1))
                                        (sfv/item (sfv/integer 2))
                                        (sfv/item (sfv/integer 3))]))))

    (is (= "\"foo\", \"bar\""
           (sfv/serialize (sfv/sf-list [(sfv/item (sfv/string "foo"))
                                        (sfv/item (sfv/string "bar"))]))))))

(deftest serialize-inner-list-test
  (testing "Inner list in a list"
    (is (= "(1 2), 3"
           (sfv/serialize (sfv/sf-list [(sfv/inner-list [(sfv/item (sfv/integer 1))
                                                         (sfv/item (sfv/integer 2))])
                                        (sfv/item (sfv/integer 3))]))))

    (is (= "(\"foo\" \"bar\");lvl=5"
           (sfv/serialize (sfv/sf-list [(sfv/inner-list [(sfv/item (sfv/string "foo"))
                                                         (sfv/item (sfv/string "bar"))]
                                                        (sfv/params "lvl" (sfv/integer 5)))]))))))

(deftest serialize-dictionary-test
  (testing "Simple dictionary"
    (is (= "a=1, b=2"
           (sfv/serialize (sfv/sf-dict [["a" (sfv/item (sfv/integer 1))]
                                        ["b" (sfv/item (sfv/integer 2))]]))))

    (is (= "a, b=?0"
           (sfv/serialize (sfv/sf-dict [["a" (sfv/item (sfv/bool true))]
                                        ["b" (sfv/item (sfv/bool false))]]))))

    (is (= "a=(1 2), b=3"
           (sfv/serialize (sfv/sf-dict [["a" (sfv/inner-list [(sfv/item (sfv/integer 1))
                                                              (sfv/item (sfv/integer 2))])]
                                        ["b" (sfv/item (sfv/integer 3))]]))))))

(deftest round-trip-test
  (testing "Parse and serialize round-trip"
    (let [test-cases [["item" "42"]
                      ["item" "\"hello\""]
                      ["item" "?1"]
                      ["item" "foo"]
                      ["item" "42;a=1"]
                      ["list" "1, 2, 3"]
                      ["list" "(1 2), 3"]
                      ["dictionary" "a=1, b=2"]]]
      (doseq [[field-type input] test-cases]
        (testing (str "Round-trip " field-type ": " input)
          (let [parsed (sfv/parse field-type input)
                serialized (sfv/serialize parsed)]
            (is (= input serialized))))))))