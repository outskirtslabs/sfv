(ns ol.sfv.conformance-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.pprint :as pp]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [charred.api :as json]
            [ol.sfv.impl :as impl]))

(defn load-test-cases
  "Load test cases from a JSON file in the structured-field-tests directory"
  [filename]
  (with-open [reader (io/reader (io/resource (str "fixtures/" filename)))]
    (json/read-json reader :key-fn keyword)))

(defn base32->bytes
  "Decode Base32 to bytes using Apache Commons Codec"
  [base32-str]
  (if (str/blank? base32-str)
    (byte-array 0)
    (let [codec (org.apache.commons.codec.binary.Base32.)]
      (.decode codec ^String base32-str))))

(defn expected-value->clojure
  "Convert the expected value format from the test suite to our Clojure representation"
  [value]
  (cond
    ;; Handle tokens with __type metadata
    (and (map? value) (= (:__type value) "token"))
    {:type :token :value (:value value)}

    ;; Handle binary with __type metadata - decode base32 to actual bytes for comparison
    (and (map? value) (= (:__type value) "binary"))
    {:type :bytes :value (base32->bytes (:value value))}

    ;; Handle display strings with __type metadata
    (and (map? value) (= (:__type value) "displaystring"))
    {:type :dstring :value (:value value)}

    ;; Handle dates with __type metadata
    (and (map? value) (= (:__type value) "date"))
    {:type :date :value (:value value)}

    ;; Handle bare primitives
    (string? value) {:type :string :value value}
    (integer? value) {:type :integer :value value}
    (float? value) {:type :decimal :value (bigdec value)} ; Convert to BigDecimal for consistency
    (true? value) {:type :boolean :value true}
    (false? value) {:type :boolean :value false}

    ;; Handle vectors (parameters or inner lists)
    (vector? value)
    (mapv expected-value->clojure value)

    ;; Pass through anything else
    :else value))

(defn expected-params->clojure
  "Convert parameter format from test suite to our representation"
  [params]
  (mapv (fn [[k v]] [k (expected-value->clojure v)]) params))

(defn expected-item->clojure
  "Convert an item format from test suite to our representation"
  [[bare-item params]]
  {:type :item
   :bare (expected-value->clojure bare-item)
   :params (expected-params->clojure params)})

(defn expected-inner-list->clojure
  "Convert an inner list format from test suite to our representation"
  [[items params]]
  {:type :inner-list
   :items (mapv expected-item->clojure items)
   :params (expected-params->clojure params)})

(defn expected-list->clojure
  "Convert a list format from test suite to our representation"
  [expected]
  {:type :list
   :members (mapv (fn [member]
                    (let [[first-elem] member]
                      (if (vector? first-elem)
                        ;; It's an inner list
                        (expected-inner-list->clojure member)
                        ;; It's a regular item
                        (expected-item->clojure member))))
                  expected)})

(defn expected-dict->clojure
  "Convert a dictionary format from test suite to our representation"
  [expected]
  {:type :dict
   :entries (mapv (fn [[key [value params]]]
                    [key (if (vector? value)
                           ;; It's an inner list
                           {:type :inner-list
                            :items (mapv expected-item->clojure value)
                            :params (expected-params->clojure params)}
                           ;; It's a regular item  
                           {:type :item
                            :bare (expected-value->clojure value)
                            :params (expected-params->clojure params)})])
                  expected)})

(defn norm [data]
  (walk/postwalk #(if (bytes? %) (seq %) %) data))

(defn run-parse-test
  "Run a single parsing test case"
  [{:keys [name raw header_type expected must_fail] :as t}]
  (testing (str "Parse test: " name)
    (println "conformance test definition:")
    (pp/pprint t)
    (let [input (if (= 1 (count raw))
                  (first raw)
                  (impl/combine-field-lines raw))] ; Combine multiple lines per RFC
      (if must_fail
        (is (thrown? Exception (impl/parse header_type input))
            (str "Expected parse to fail for: " name))
        (let [result (impl/parse header_type input)
              expected-clojure (case header_type
                                 "item" (expected-item->clojure expected)
                                 "list" (expected-list->clojure expected)
                                 "dictionary" (expected-dict->clojure expected))]
          (is (= (norm expected-clojure) (norm result))
              (str "Parse result mismatch for: " name
                   "\nExpected: " (pr-str expected-clojure)
                   "\nActual: " (pr-str result))))))))

(defn run-conformance-tests
  "Run all test cases from a JSON file"
  [filename]
  (let [test-cases (load-test-cases filename)]
    (doseq [test-case test-cases]
      (run-parse-test test-case))))

;; Define test functions for each test file

(deftest conformance-examples-test
  (run-conformance-tests "examples.json"))

(deftest conformance-item-test
  (run-conformance-tests "item.json"))

(deftest conformance-list-test
  (run-conformance-tests "list.json"))

(deftest conformance-dictionary-test
  (run-conformance-tests "dictionary.json"))

(deftest conformance-boolean-test
  (run-conformance-tests "boolean.json"))

(deftest conformance-number-test
  (run-conformance-tests "number.json"))

(deftest conformance-string-test
  (run-conformance-tests "string.json"))

(deftest conformance-token-test
  (run-conformance-tests "token.json"))

(deftest conformance-binary-test
  (run-conformance-tests "binary.json"))

(deftest conformance-date-test
  (run-conformance-tests "date.json"))

(deftest conformance-display-string-test
  (run-conformance-tests "display-string.json"))

(deftest conformance-param-dict-test
  (run-conformance-tests "param-dict.json"))

(deftest conformance-param-list-test
  (run-conformance-tests "param-list.json"))

(deftest conformance-param-listlist-test
  (run-conformance-tests "param-listlist.json"))

(deftest conformance-listlist-test
  (run-conformance-tests "listlist.json"))
