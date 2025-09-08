;; Copyright © 2025 Casey Link <casey@outskirtslabs.com>
;; SPDX-License-Identifier: MIT
(ns ol.sfv.error-test
  (:require [clojure.test :refer [deftest is testing are]]
            [ol.sfv :as sfv]))

(deftest diagnostics-test
  (testing "Error diagnostics with position and message information"
    (are [input field-type expected-pos expected-msg-prefix]
         (try
           (sfv/parse field-type input)
           (is false (str "Should have failed parsing: " input))
           (catch clojure.lang.ExceptionInfo ex
             (let [data (ex-data ex)]
               (is (= true (:ol.sfv/error data)) "Should be a SFV error")
               (is (= expected-pos (:pos data)) (str "Position mismatch for: " input))
               (is (.startsWith (ex-message ex) expected-msg-prefix)
                   (str "Message mismatch for: " input "\nExpected prefix: " expected-msg-prefix "\nActual: " (ex-message ex))))))

      ;; Invalid characters in field line
      "\u0080" nil 0 "parse error:"
      "   \u0080" nil 3 "parse error:"

      ;; Integer/decimal parsing errors  
      "-a" "item" 1 "parse error: Expected DIGIT"
      "1234567890123.2" "item" 13 "parse error: Integer part too long"
      "12345678901234567" "item" 15 "parse error: Integer too long"
      "123456789012.5556" "item" 16 "parse error: Decimal too long"
      "123456789012." "item" 12 "parse error: Decimal has trailing dot"
      "0.1234" "item" 5 "parse error: Decimal fraction too long"

      ;; String parsing errors
      "\"\\" "item" 2 "parse error: Unterminated escape sequence"
      "\"\\a\"" "item" 2 "parse error: Invalid escape sequence"
      "\"\u007f\"" "item" 2 "parse error: Invalid character in string"
      "\"incomplete" "item" 11 "parse error: Unterminated string"

      ;; Byte sequence errors
      ":empty" "item" 6 "parse error: Missing closing ':'"
      ":em pty:" "item" 4 "parse error: Invalid base64 character"
      ":empty:" "item" 7 "parse error: Invalid base64 encoding"

      ;; Boolean errors  
      "?" "item" 1 "parse error: Expected '0' or '1'"
      "??" "item" 1 "parse error: Expected '0' or '1'"

      ;; Parameter/key errors
      "1;" "item" 2 "parse error: Invalid key start"
      "1;_" "item" 2 "parse error: Invalid key start"

      ;; Bare item type errors
      "<uri>" "item" 0 "parse error: Unrecognized bare item type"

      ;; List parsing errors
      "1 2" "list" 2 "parse error:"
      "1, 2," "list" 5 "parse error:"
      "(1 2" "list" 4 "parse error: Unterminated inner list"
      "(1 2#" "list" 4 "parse error: Expected SP or ')'"

      ;; Dictionary parsing errors  
      "a b" "dictionary" 2 "parse error:"
      "a,b," "dictionary" 4 "parse error:"

      ;; Date parsing errors
      "@12.34" "item" 3 "parse error: Date must be an integer"

      ;; Display string errors
      "%\"" "item" 2 "parse error: Display String missing closing quote"
      "%\"nonhex percent: %XX\"" "item" 19 "parse error: Invalid hex digit"
      "%\"truncated UTF-8: %80\"" "item" 20 "parse error: Invalid UTF-8 sequence"
      "%\"surrogate: %ed%ba%ad\"" "item" 13 "parse error: Invalid UTF-8 sequence"
      "%\"invalid UTF-8 (RFC 3629, Section 3): %c0%80\"" "item" 39 "parse error: Invalid UTF-8 sequence"
      "%\"invalid UTF-8 (RFC 3629, Section 3): %ed%a1%8c%ed%be%b4\"" "item" 41 "parse error: Invalid UTF-8 sequence"
      "%\"mix:%20%20%ff\"" "item" 12 "parse error: Invalid UTF-8 sequence")))