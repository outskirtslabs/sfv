(ns ol.sfv.parser-test
  (:require [clojure.test :refer [deftest is testing are]]
            [ol.sfv.impl :as impl]))

(deftest test-character-predicates
  (testing "digit?"
    (is (impl/digit? \0))
    (is (impl/digit? \9))
    (is (not (impl/digit? \a)))
    (is (not (impl/digit? \space))))

  (testing "alpha?"
    (is (impl/alpha? \A))
    (is (impl/alpha? \z))
    (is (not (impl/alpha? \0)))
    (is (not (impl/alpha? \space))))

  (testing "lcalpha?"
    (is (impl/lcalpha? \a))
    (is (impl/lcalpha? \z))
    (is (not (impl/lcalpha? \A)))
    (is (not (impl/lcalpha? \0))))

  (testing "tchar?"
    (is (impl/tchar? \a))
    (is (impl/tchar? \A))
    (is (impl/tchar? \0))
    (is (impl/tchar? \!))
    (is (not (impl/tchar? \space)))
    (is (not (impl/tchar? \"))))

  (testing "lc-hexdig?"
    (is (impl/lc-hexdig? \0))
    (is (impl/lc-hexdig? \9))
    (is (impl/lc-hexdig? \a))
    (is (impl/lc-hexdig? \f))
    (is (not (impl/lc-hexdig? \A)))
    (is (not (impl/lc-hexdig? \g)))
    (is (not (impl/lc-hexdig? \space))))

  (testing "hex-digit-value"
    (is (= 0 (impl/hex-digit-value \0)))
    (is (= 9 (impl/hex-digit-value \9)))
    (is (= 10 (impl/hex-digit-value \a)))
    (is (= 15 (impl/hex-digit-value \f)))
    (is (thrown? IllegalArgumentException (impl/hex-digit-value \g)))
    (is (thrown? IllegalArgumentException (impl/hex-digit-value \A))))

  (testing "token-char?"
    (is (impl/token-char? \a))
    (is (impl/token-char? \:))
    (is (impl/token-char? \/))
    (is (not (impl/token-char? \space)))
    (is (not (impl/token-char? \")))))

(deftest test-scanner-utilities
  (testing "ascii-string conversion"
    (is (= "hello" (impl/ascii-string "hello")))
    (is (= "hello" (impl/ascii-string (.getBytes "hello" "ASCII"))))
    (is (thrown-with-msg? Exception #"Non-ASCII" (impl/ascii-string "héllo"))))

  (testing "init-ctx"
    (let [ctx (impl/init-ctx "hello")]
      (is (= "hello" (:s ctx)))
      (is (= 0 (:i ctx)))
      (is (= 5 (:n ctx)))))

  (testing "peek-char"
    (let [ctx (impl/init-ctx "hello")]
      (is (= \h (impl/peek-char ctx)))
      (is (= \h (impl/peek-char (assoc ctx :i 0))))
      (is (= \e (impl/peek-char (assoc ctx :i 1))))
      (is (nil? (impl/peek-char (assoc ctx :i 5))))))

  (testing "consume-char"
    (let [ctx (impl/init-ctx "hello")
          [ch ctx'] (impl/consume-char ctx)]
      (is (= \h ch))
      (is (= 1 (:i ctx')))
      (is (= "hello" (:s ctx')))
      (is (= 5 (:n ctx')))))

  (testing "eof?"
    (let [ctx (impl/init-ctx "hi")]
      (is (not (impl/eof? ctx)))
      (is (not (impl/eof? (assoc ctx :i 1))))
      (is (impl/eof? (assoc ctx :i 2)))))

  (testing "skip-ows"
    (let [ctx (impl/init-ctx "  hello")]
      (is (= 2 (:i (impl/skip-ows ctx)))))
    (let [ctx (impl/init-ctx "hello")]
      (is (= 0 (:i (impl/skip-ows ctx))))))

  (testing "skip-sp"
    (let [ctx (impl/init-ctx " hello")]
      (is (= 1 (:i (impl/skip-sp ctx)))))
    (let [ctx (impl/init-ctx "hello")]
      (is (= 0 (:i (impl/skip-sp ctx)))))))

(deftest test-parse-key
  (testing "valid keys"
    (are [input expected] (let [[key ctx] (impl/parse-key (impl/init-ctx input))]
                            (and (= expected key)
                                 (= (count input) (:i ctx))))
      "a" "a"
      "abc" "abc"
      "a1" "a1"
      "a-b" "a-b"
      "a_b" "a_b"
      "*" "*"
      "*abc" "*abc"))

  (testing "invalid keys"
    (are [input] (thrown-with-msg? Exception #"parse error"
                                   (impl/parse-key (impl/init-ctx input)))
      "A" ; uppercase start
      "1a" ; digit start
      "-a" ; dash start
      "" ; empty
      " a"))) ; space start

(deftest test-parse-integer-or-decimal
  (testing "integers"
    (are [input expected-val] (let [[num _ctx] (impl/parse-integer-or-decimal (impl/init-ctx input))]
                                (and (= :integer (:type num))
                                     (= expected-val (:value num))))
      "0" 0
      "123" 123
      "-42" -42
      "999999999999999" 999999999999999))

  (testing "decimals"
    (are [input expected-val] (let [[num _ctx] (impl/parse-integer-or-decimal (impl/init-ctx input))]
                                (and (= :decimal (:type num))
                                     (= expected-val (:value num))))
      "1.0" 1.0M
      "3.14" 3.14M
      "-2.5" -2.5M
      "0.123" 0.123M))

  (testing "invalid numbers"
    (are [input] (thrown-with-msg? Exception #"parse error"
                                   (impl/parse-integer-or-decimal (impl/init-ctx input)))
      ""
      "abc"
      "1."
      ".1"
      "1.2.3")))

(deftest test-parse-string
  (testing "basic strings"
    (are [input expected] (let [[s _ctx] (impl/parse-string (impl/init-ctx input))]
                            (and (= :string (:type s))
                                 (= expected (:value s))))
      "\"hello\"" "hello"
      "\"\"" ""
      "\"a b c\"" "a b c"))

  (testing "escaped strings"
    (are [input expected] (let [[s _ctx] (impl/parse-string (impl/init-ctx input))]
                            (and (= :string (:type s))
                                 (= expected (:value s))))
      "\"\\\"\"" "\""
      "\"\\\\\"" "\\"
      "\"a\\\"b\"" "a\"b"))

  (testing "invalid strings"
    (are [input] (thrown-with-msg? Exception #"parse error"
                                   (impl/parse-string (impl/init-ctx input)))
      "hello" ; no quotes
      "\"hello" ; no closing quote
      "\"\\x\""))) ; invalid escape

(deftest test-parse-token
  (testing "valid tokens"
    (are [input expected] (let [[token _ctx] (impl/parse-token (impl/init-ctx input))]
                            (and (= :token (:type token))
                                 (= expected (:value token))))
      "token" "token"
      "a" "a"
      "*token" "*token"
      "a:b" "a:b"
      "a/b" "a/b"
      "a-b_c1" "a-b_c1"))

  (testing "invalid tokens"
    (are [input] (thrown-with-msg? Exception #"parse error"
                                   (impl/parse-token (impl/init-ctx input)))
      "1token" ; starts with digit
      "" ; empty
      " token"))) ; starts with space

(deftest test-parse-byte-sequence
  (testing "valid byte sequences"
    (are [input expected] (let [[bytes _ctx] (impl/parse-byte-sequence (impl/init-ctx input))]
                            (and (= :bytes (:type bytes))
                                 (java.util.Arrays/equals expected (:value bytes))))
      ":aGVsbG8=:" (.getBytes "hello" "UTF-8")
      "::" (byte-array 0)
      ":YQ==:" (.getBytes "a" "UTF-8")))

  (testing "invalid byte sequences"
    (are [input] (thrown-with-msg? Exception #"parse error"
                                   (impl/parse-byte-sequence (impl/init-ctx input)))
      "hello" ; no colons
      ":hello" ; no closing colon  
      ":@#$:"))) ; invalid base64

(deftest test-parse-sfv-boolean
  (testing "valid booleans"
    (are [input expected] (let [[bool _ctx] (impl/parse-sfv-boolean (impl/init-ctx input))]
                            (and (= :boolean (:type bool))
                                 (= expected (:value bool))))
      "?1" true
      "?0" false))

  (testing "invalid booleans"
    (are [input] (thrown-with-msg? Exception #"parse error"
                                   (impl/parse-sfv-boolean (impl/init-ctx input)))
      "?2"
      "?true"
      "1"
      "?")))

(deftest test-parse-date
  (testing "valid dates"
    (are [input expected] (let [[date _ctx] (impl/parse-date (impl/init-ctx input))]
                            (and (= :date (:type date))
                                 (= expected (:value date))))
      "@1234567890" 1234567890
      "@0" 0
      "@-1" -1))

  (testing "invalid dates"
    (are [input] (thrown-with-msg? Exception #"parse error"
                                   (impl/parse-date (impl/init-ctx input)))
      "1234567890" ; no @
      "@abc" ; not a number
      "@"))) ; no number

(deftest test-parse-display-string
  (testing "basic display strings"
    (are [input expected] (let [[dstr _ctx] (impl/parse-display-string (impl/init-ctx input))]
                            (and (= :dstring (:type dstr))
                                 (= expected (:value dstr))))
      "%\"hello\"" "hello"
      "%\"\"" ""
      "%\"Hello World\"" "Hello World"))

  (testing "percent-encoded display strings"
    (are [input expected] (let [[dstr _ctx] (impl/parse-display-string (impl/init-ctx input))]
                            (and (= :dstring (:type dstr))
                                 (= expected (:value dstr))))
      "%\"hello%20world\"" "hello world"
      "%\"a%25b\"" "a%b"))

  (testing "invalid display strings"
    (are [input] (thrown-with-msg? Exception #"parse error"
                                   (impl/parse-display-string (impl/init-ctx input)))
      "\"hello\"" ; no %
      "%hello" ; no quotes
      "%\"hello"))) ; no closing quote

(deftest test-parse-bare-item
  (testing "dispatch to correct parser"
    (let [test-cases [["123" :integer]
                      ["1.5" :decimal]
                      ["\"hello\"" :string]
                      ["token" :token]
                      [":YWJj:" :bytes]
                      ["?1" :boolean]
                      ["@1234567890" :date]
                      ["%\"hello\"" :dstring]]]
      (doseq [[input expected-type] test-cases]
        (let [[item _ctx] (impl/parse-bare-item (impl/init-ctx input))]
          (is (= expected-type (:type item))
              (str "Expected " expected-type " for input " input ", got " (:type item))))))))