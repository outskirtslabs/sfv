(ns ol.sfv.impl
  (:refer-clojure :exclude [integer? decimal? string? bytes bytes?])
  (:require [clojure.string :as str])
  (:import (java.nio.charset StandardCharsets)
           (java.util Base64)))

;; Minimal implementation scaffold for RFC9651 parsing in ol.sfv.impl
;; This file provides core helpers, char classes, parse-key, and stubs
;; for the concrete parsers (to be implemented next). Each parser will
;; follow the exact RFC algorithms (references added where relevant).

;; ---------------------------------------------------------------------------
;; Data constructors (tagged maps)

(defn token [s] {:type :token :value s})
(defn token? [x] (= (:type x) :token))

(defn decimal [x] {:type :decimal :value x})
(defn decimal? [x] (= (:type x) :decimal))

(defn integer [n] {:type :integer :value n})
(defn integer? [x] (= (:type x) :integer))

(defn string [s] {:type :string :value s})
(defn string? [x] (= (:type x) :string))

(defn dstring [s] {:type :dstring :value s})
(defn dstring? [x] (= (:type x) :dstring))

(defn bytes [b] {:type :bytes :value b})
(defn bytes? [x] (= (:type x) :bytes))

(defn bool [b] {:type :boolean :value (boolean b)})
(defn bool? [x] (= (:type x) :boolean))

(defn date [secs] {:type :date :value secs})
(defn date? [x] (= (:type x) :date))

;; Parameters (ordered vector of [k bare-item])
(defn params [& kvs]
  (cond
    (and (= 1 (count kvs)) (sequential? (first kvs))) (vec (first kvs))
    :else (loop [xs kvs acc []]
            (if (empty? xs)
              acc
              (let [[k v & rest] xs]
                (recur rest (conj acc [k v])))))))

(defn param-get [ps k] (some (fn [[kk vv]] (when (= kk k) vv)) ps))
(defn param-keys [ps] (mapv first ps))

;; Item/Inner/List/Dict constructors (lightweight wrappers)
(defn item
  ([bare] {:type :item :bare bare :params []})
  ([bare ps] {:type :item :bare bare :params ps}))
(defn item? [x] (= (:type x) :item))

(defn item-bare [i] (:bare i))
(defn item-params [i] (:params i))

(defn inner-list
  ([items] {:type :inner-list :items items :params []})
  ([items ps] {:type :inner-list :items items :params ps}))

(defn inner-list? [x] (= (:type x) :inner-list))
(defn inner-items [il] (:items il))
(defn inner-params [il] (:params il))

(defn sf-list [members] {:type :list :members members})

(defn sf-list? [x] (= (:type x) :list))
(defn list-members [l] (:members l))
(defn sf-dict [entries] {:type :dict :entries entries})

(defn sf-dict? [x] (= (:type x) :dict))
(defn dict-keys [d] (mapv first (:entries d)))
(defn dict-get [d k] (some (fn [[kk vv]] (when (= kk k) vv)) (:entries d)))
(defn dict->pairs [d] (:entries d))

(defn flag
  ([] {:type :flag :params []})
  ([ps] {:type :flag :params ps}))

(defn flag? [x] (= (:type x) :flag))

;; ---------------------------------------------------------------------------
;; Scanner / cursor utilities

(defn ascii-string [s]
  (cond
    (clojure.core/string? s)
    (do (doseq [ch s]
          (when (> (int ch) 127)
            (throw (ex-info "Non-ASCII character in string" {:input s}))))
        s)
    (instance? (Class/forName "[B") s)
    (String. ^bytes s StandardCharsets/US_ASCII)
    :else (throw (ex-info "ascii-string: expected string or bytes" {:input s}))))

(defn init-ctx [s-or-bytes]
  (let [s2 (ascii-string s-or-bytes)]
    {:s s2 :i 0 :n (.length ^String s2)}))

(defn eof? [ctx] (>= (:i ctx) (:n ctx)))
(defn peek-char [ctx]
  (let [i (:i ctx) s (:s ctx) n (:n ctx)]
    (when (< i n) (.charAt ^String s i))))
(defn consume-char [ctx]
  (if (eof? ctx) [nil ctx] (let [ch (peek-char ctx)] [ch (update ctx :i inc)])))

(defn skip-ows [ctx]
  (loop [c ctx]
    (let [ch (peek-char c)]
      (if (and ch (or (= ch \space) (= ch \tab))) (recur (update c :i inc)) c))))

(defn skip-sp [ctx]
  (loop [c ctx seen false]
    (let [ch (peek-char c)]
      (if (and ch (= ch \space)) (recur (update c :i inc) true) (if seen c c)))))

;; ---------------------------------------------------------------------------
;; Character predicates (use explicit int comparisons to satisfy linter)

(defn digit? [ch]
  (and ch (<= (int \0) (int ch) (int \9))))

(defn alpha? [ch]
  (and ch (or (<= (int \a) (int ch) (int \z))
              (<= (int \A) (int ch) (int \Z)))))

(defn lcalpha? [ch]
  (and ch (<= (int \a) (int ch) (int \z))))

(def ^:private tchar-set
  #{\! \# \$ \% \& \' \* \+ \- \. \^ \_ \` \| \~})
(defn tchar? [ch] (or (digit? ch) (alpha? ch) (contains? tchar-set ch)))
(defn token-char? [ch] (or (tchar? ch) (= ch \:) (= ch \/)))

;; Helper for hex digit validation and conversion
(defn lc-hexdig? [ch]
  (and ch (or (<= (int \0) (int ch) (int \9))
              (<= (int \a) (int ch) (int \f)))))

(defn hex-digit-value [ch]
  (cond
    (<= (int \0) (int ch) (int \9)) (- (int ch) (int \0))
    (<= (int \a) (int ch) (int \f)) (+ (- (int ch) (int \a)) 10)
    :else (throw (IllegalArgumentException. (str "Invalid hex digit: " ch)))))

(defn parse-error [ctx reason & {:keys [found expected]}]
  (throw (ex-info (str "parse error: " reason) (merge {:ol.sfv/error true :pos (:i ctx)
                                                       (when found {:found found})
                                                       (when expected {:expected expected})}))))

;; ---------------------------------------------------------------------------
;; parse-key (RFC 9651 §4.2.3.3)

(defn parse-key [ctx]
  (let [ch (peek-char ctx)]
    (when-not (and ch (or (lcalpha? ch) (= ch \*)))
      (parse-error ctx "Invalid key start" :found ch :expected "lcalpha or '*'"))
    (let [s (:s ctx) n (:n ctx) i (:i ctx) sb (StringBuilder.)]
      (.append sb (peek-char ctx))
      (loop [i (inc i)]
        (if (< i n)
          (let [ch (.charAt ^String s i)]
            (if (or (lcalpha? ch)
                    (<= (int \0) (int ch) (int \9))
                    (= ch \_)
                    (= ch \-)
                    (= ch \.)
                    (= ch \*))
              (do (.append sb ch) (recur (inc i)))
              (let [key (.toString sb) ctx' (assoc ctx :i i)] [key ctx'])))
          (let [key (.toString sb) ctx' (assoc ctx :i n)] [key ctx']))))))

;; ---------------------------------------------------------------------------
;; Parser: parse-integer-or-decimal (RFC 9651 §4.2.4)
;; Implements algorithm in 4.2.4 exactly, including digit limits.

(defn parse-integer-or-decimal [ctx]
  (let [s (:s ctx) n (:n ctx) i0 (:i ctx)]
    ;; Step 4: optional sign
    (when (>= i0 n) (parse-error ctx "empty integer/decimal"))
    (let [first-ch (.charAt ^String s i0)
          [sign i] (if (= first-ch \-) [-1 (inc i0)] [1 i0])]
      (when (>= i n) (parse-error ctx "empty integer after sign"))
      (let [first-ch2 (.charAt ^String s i)]
        (when-not (digit? first-ch2) (parse-error ctx "Expected DIGIT" :found first-ch2 :expected "DIGIT"))
        (let [sb (StringBuilder.)]
          (loop [i i type :integer sb sb]
            (if (< i n)
              (let [ch (.charAt ^String s i)]
                (cond
                  (digit? ch)
                  (do (.append sb ch)
                      (let [len (.length sb)]
                        (when (and (= type :integer) (> len 15)) (parse-error ctx "Integer too long"))
                        (when (and (= type :decimal) (> len 16)) (parse-error ctx "Decimal too long"))
                        (recur (inc i) type sb)))

                  (and (= type :integer) (= ch \.))
                  (do (when (> (.length sb) 12) (parse-error ctx "Integer part too long for decimal"))
                      (.append sb ch)
                      (when (> (.length sb) 16) (parse-error ctx "Decimal too long"))
                      (recur (inc i) :decimal sb))

                  (and (= type :decimal) (= ch \.))
                  (parse-error ctx "Multiple decimal points not allowed" :found ch :expected "DIGIT")

                  :else
                  (let [input-number (.toString sb) ctx' (assoc ctx :i i)]
                    (if (= type :integer)
                      (let [num (Long/parseLong input-number)] [{:type :integer :value (* sign num)} ctx'])
                      (do (when (.endsWith input-number ".") (parse-error ctx "Decimal has trailing dot"))
                          (let [dot-pos (.indexOf input-number ".")
                                frac-len (if (neg? dot-pos) 0 (- (.length input-number) (inc dot-pos)))]
                            (when (> frac-len 3) (parse-error ctx "Decimal fraction too long"))
                            (let [bd (java.math.BigDecimal. input-number)
                                  bd (if (= sign -1) (.negate bd) bd)]
                              [{:type :decimal :value bd} ctx'])))))))
              ;; end-of-input
              (let [input-number (.toString sb) ctx' (assoc ctx :i i)]
                (if (str/blank? input-number)
                  (parse-error ctx "No digits parsed for number")
                  (do (when (.endsWith input-number ".") (parse-error ctx "Decimal has trailing dot"))
                      (let [dot-pos (.indexOf input-number ".")]
                        (if (= dot-pos -1)
                          (let [num (Long/parseLong input-number)] [{:type :integer :value (* sign num)} ctx'])
                          (let [frac-len (- (.length input-number) (inc dot-pos))]
                            (when (> frac-len 3) (parse-error ctx "Decimal fraction too long"))
                            (let [bd (java.math.BigDecimal. input-number)
                                  bd (if (= sign -1) (.negate bd) bd)]
                              [{:type :decimal :value bd} ctx']))))))))))))))

;; The remaining parsers (parse-string, parse-token, parse-byte-sequence,
;; parse-boolean, parse-date, parse-display-string, parse-parameters,
;; item/inner-list/list/dictionary) will be implemented  following
;; the exact step-by-step algorithms in RFC 9651 (Section 4.2 and 4.1).

;; Stub other parsers for now (kept until implemented):
(defn parse-string [ctx]
  ;; RFC 9651 §4.2.5: Parsing a String
  (let [sb (StringBuilder.)
        ch (peek-char ctx)]
    (when-not (= ch \")
      (parse-error ctx "Expected opening DQUOTE for string" :found ch :expected "\""))
    (let [[_ ctx'] (consume-char ctx)]
      (loop [ctx' ctx']
        (if (eof? ctx')
          (parse-error ctx' "Unterminated string")
          (let [[char ctx''] (consume-char ctx')]
            (cond
              (= char \\)
              (if (eof? ctx'')
                (parse-error ctx'' "Unterminated escape sequence")
                (let [[next-char ctx'''] (consume-char ctx'')]
                  (if (or (= next-char \") (= next-char \\))
                    (do (.append sb next-char)
                        (recur ctx'''))
                    (parse-error ctx''' "Invalid escape sequence" :found next-char :expected "\" or \\"))))

              (= char \")
              [{:type :string :value (.toString sb)} ctx'']

              (or (<= 0 (int char) 0x1F) (<= 0x7F (int char) 0xFF))
              (parse-error ctx'' "Invalid character in string" :found char :expected "VCHAR or SP")

              :else
              (do (.append sb char)
                  (recur ctx'')))))))))
(defn parse-token [ctx]
  ;; RFC 9651 §4.2.6: Parsing a Token
  ;; 1. If the first character of input_string is not ALPHA or "*", fail parsing.
  (let [ch (peek-char ctx)]
    (when-not (and ch (or (alpha? ch) (= ch \*)))
      (parse-error ctx "Invalid token start" :found ch :expected "ALPHA or '*'"))
    ;; 2. Let output_string be an empty string.
    (let [sb (StringBuilder.)]
      ;; 3. While input_string is not empty:
      (loop [ctx' ctx]
        (if (eof? ctx')
          ;; 4. Return output_string.
          [{:type :token :value (.toString sb)} ctx']
          ;; 3.1. If the first character of input_string is not in tchar, ":", or "/", return output_string.
          (let [ch (peek-char ctx')]
            (if (token-char? ch)
              ;; 3.2. Let char be the result of consuming the first character of input_string.
              (let [[char ctx''] (consume-char ctx')]
                ;; 3.3. Append char to output_string.
                (.append sb char)
                (recur ctx''))
              ;; Return when we hit a non-token character
              [{:type :token :value (.toString sb)} ctx'])))))))
(defn parse-byte-sequence [ctx]
  ;; RFC 9651 §4.2.7: Parsing a Byte Sequence
  ;; 1. If the first character of input_string is not ":", fail parsing.
  (let [ch (peek-char ctx)]
    (when-not (= ch \:)
      (parse-error ctx "Expected opening ':' for byte sequence" :found ch :expected ":"))
    ;; 2. Discard the first character of input_string.
    (let [[_ ctx'] (consume-char ctx)
          s (:s ctx')
          i (:i ctx')
          end-colon-pos (.indexOf ^String s ":" i)]
      ;; 3. If there is not a ":" character before the end of input_string, fail parsing.
      (when (= end-colon-pos -1)
        (parse-error ctx' "Missing closing ':' for byte sequence"))
      ;; 4. Let b64_content be the result of consuming content up to but not including the first ":".
      (let [b64-content (.substring ^String s i end-colon-pos)
            ctx'' (assoc ctx' :i (inc end-colon-pos))] ;; 5. Consume the ":" character
        ;; 6. If b64_content contains a character not in ALPHA, DIGIT, "+", "/", and "=", fail parsing.
        (doseq [ch b64-content]
          (when-not (or (alpha? ch) (digit? ch) (= ch \+) (= ch \/) (= ch \=))
            (parse-error ctx' "Invalid base64 character" :found ch :expected "ALPHA, DIGIT, '+', '/', or '='")))
        ;; 7. Let binary_content be the result of base64-decoding, synthesizing padding if necessary.
        (try
          (let [decoder (Base64/getDecoder)
                binary-content (.decode decoder ^String b64-content)]
            ;; 8. Return binary_content.
            [{:type :bytes :value binary-content} ctx''])
          (catch Exception _
            (parse-error ctx' "Invalid base64 encoding")))))))
(defn parse-sfv-boolean [ctx]
  ;; RFC 9651 §4.2.8: Parsing a Boolean
  ;; 1. If the first character of input_string is not "?", fail parsing.
  (let [ch (peek-char ctx)]
    (when-not (= ch \?)
      (parse-error ctx "Expected '?' for boolean" :found ch :expected "?"))
    ;; 2. Discard the first character of input_string.
    (let [[_ ctx'] (consume-char ctx)
          ch2 (peek-char ctx')]
      (cond
        ;; 3. If the first character matches "1", discard and return true.
        (= ch2 \1)
        (let [[_ ctx''] (consume-char ctx')]
          [{:type :boolean :value true} ctx''])
        ;; 4. If the first character matches "0", discard and return false.
        (= ch2 \0)
        (let [[_ ctx''] (consume-char ctx')]
          [{:type :boolean :value false} ctx''])
        ;; 5. No value has matched; fail parsing.
        :else
        (parse-error ctx' "Expected '0' or '1' after '?'" :found ch2 :expected "0 or 1")))))
(defn parse-date [ctx]
  ;; RFC 9651 §4.2.9: Parsing a Date
  ;; 1. If the first character of input_string is not "@", fail parsing.
  (let [ch (peek-char ctx)]
    (when-not (= ch \@)
      (parse-error ctx "Expected '@' for date" :found ch :expected "@"))
    ;; 2. Discard the first character of input_string.
    (let [[_ ctx'] (consume-char ctx)
          ;; 3. Let output_date be the result of running Parsing an Integer or Decimal with input_string.
          [output-date ctx''] (parse-integer-or-decimal ctx')]
        ;; 4. If output_date is a Decimal, fail parsing.
      (when (= (:type output-date) :decimal)
        (parse-error ctx'' "Date must be an integer, not decimal"))
        ;; 5. Return output_date.
      [{:type :date :value (:value output-date)} ctx''])))

(defn decode-percent-sequence
  "Decode a percent-encoded sequence (%XX) into a byte value.
   Returns [byte new-ctx] or throws parse error."
  [ctx]
  (let [[c1 ctx'] (consume-char ctx)]
    (when-not (lc-hexdig? c1)
      (parse-error ctx' "Invalid hex digit in percent sequence"
                   :found c1 :expected "lowercase hex digit"))
    (let [[c2 ctx''] (consume-char ctx')]
      (when-not (lc-hexdig? c2)
        (parse-error ctx'' "Invalid hex digit in percent sequence"
                     :found c2 :expected "lowercase hex digit"))
      (let [byte-val (+ (* (hex-digit-value c1) 16) (hex-digit-value c2))]
        [byte-val ctx'']))))

(defn utf8-decode-bytes
  "Decode a byte array as UTF-8, returning [decoded-string nil] on success or [nil error-message] on failure."
  [byte-array]
  (try
    (let [decoder (-> StandardCharsets/UTF_8
                      .newDecoder
                      (.onMalformedInput java.nio.charset.CodingErrorAction/REPORT)
                      (.onUnmappableCharacter java.nio.charset.CodingErrorAction/REPORT))
          byte-buffer (java.nio.ByteBuffer/wrap byte-array)]
      [(-> decoder (.decode byte-buffer) .toString) nil])
    (catch java.nio.charset.CharacterCodingException e
      [nil (.getMessage e)])))

(defn parse-display-string [ctx]
  ;; RFC 9651 §4.2.10: Parsing a Display String
  ;; 1. If the first two characters of input_string are not "%" followed by DQUOTE, fail parsing.
  (let [[c1 ctx'] (consume-char ctx)]
    (when-not (= c1 \%)
      (parse-error ctx' "Display String must start with %\""
                   :found c1 :expected "%"))

    (let [[c2 ctx''] (consume-char ctx')]
      (when-not (= c2 \")
        (parse-error ctx'' "Display String must start with %\""
                     :found c2 :expected "\""))

      ;; 2. Discard the first two characters of input_string.
      ;; 3. Let byte_array be an empty byte array.
      (let [byte-array (java.io.ByteArrayOutputStream.)]

        ;; 4. While input_string is not empty:
        (loop [ctx-current ctx'']
          (if (eof? ctx-current)
            ;; 5. Reached the end of input_string without finding a closing DQUOTE; fail parsing.
            (parse-error ctx-current "Display String missing closing quote")

            (let [[char ctx-next] (consume-char ctx-current)]
              (cond
                ;; If char is DQUOTE: decode byte_array as UTF-8 and return
                (= char \")
                (let [bytes (.toByteArray byte-array)
                      [unicode-str error-msg] (utf8-decode-bytes bytes)]
                  ;; RFC step 4.4.1: "Fail parsing if decoding fails"
                  (if unicode-str
                    [{:type :dstring :value unicode-str} ctx-next]
                    (parse-error ctx-next (str "Invalid UTF-8 sequence in display string: " error-msg))))

                ;; If char is "%": decode percent sequence
                (= char \%)
                (let [[byte-val ctx-after-percent] (decode-percent-sequence ctx-next)]
                  (.write byte-array (unchecked-byte byte-val))
                  (recur ctx-after-percent))

                ;; If char is in the range %x00-1f or %x7f-ff (i.e., it is not in VCHAR or SP), fail parsing.
                (or (< (int char) 0x20) (>= (int char) 0x7f))
                (parse-error ctx-next "Invalid character in Display String"
                             :found char :expected "VCHAR or SP")

                ;; Otherwise, append char as ASCII byte to byte_array
                :else
                (do
                  (.write byte-array (int char))
                  (recur ctx-next))))))))))

(defn parse-bare-item [ctx]
  ;; RFC 9651 §4.2.3.1: Parsing a Bare Item
  (let [ch (peek-char ctx)]
    (cond
      ;; 1. If the first character is "-" or DIGIT, parse Integer or Decimal
      (or (= ch \-) (digit? ch))
      (parse-integer-or-decimal ctx)

      ;; 2. If the first character is DQUOTE, parse String
      (= ch \")
      (parse-string ctx)

      ;; 3. If the first character is ALPHA or "*", parse Token
      (or (alpha? ch) (= ch \*))
      (parse-token ctx)

      ;; 4. If the first character is ":", parse Byte Sequence
      (= ch \:)
      (parse-byte-sequence ctx)

      ;; 5. If the first character is "?", parse Boolean
      (= ch \?)
      (parse-sfv-boolean ctx)

      ;; 6. If the first character is "@", parse Date
      (= ch \@)
      (parse-date ctx)

      ;; 7. If the first character is "%", parse Display String
      (= ch \%)
      (parse-display-string ctx)

      ;; 8. Otherwise, the item type is unrecognized; fail parsing
      :else
      (parse-error ctx "Unrecognized bare item type" :found ch :expected "-, DIGIT, \", ALPHA, *, :, ?, @, or %"))))
(defn parse-parameters [ctx]
  ;; RFC 9651 §4.2.3.2: Parsing Parameters
  ;; 1. Let parameters be an empty, ordered map.
  (loop [ctx ctx parameters []]
    ;; 2. While input_string is not empty:
    (if (eof? ctx)
      ;; 3. Return parameters.
      [parameters ctx]
      (let [ch (peek-char ctx)]
        ;; 2.1. If the first character is not ";", exit the loop.
        (if-not (= ch \;)
          [parameters ctx]
          ;; 2.2. Consume the ";" character from the beginning.
          (let [[_ ctx'] (consume-char ctx)
                ;; 2.3. Discard any leading SP characters.
                ctx'' (skip-sp ctx')
                ;; 2.4. Let param_key be the result of running Parsing a Key.
                [param-key ctx'''] (parse-key ctx'')
                ;; 2.5. Let param_value be Boolean true.
                param-value {:type :boolean :value true}
                ch2 (peek-char ctx''')]
            ;; 2.6. If the first character is "=":
            (if (= ch2 \=)
              ;; 2.6.1. Consume the "=" character.
              (let [[_ ctx''''] (consume-char ctx''')
                    ;; 2.6.2. Let param_value be the result of running Parsing a Bare Item.
                    [param-value' ctx'''''] (parse-bare-item ctx'''')]
                ;; 2.7 & 2.8. Append/overwrite key param_key with value param_value (last-write-wins).
                (recur ctx''''' (conj (filterv #(not= (first %) param-key) parameters)
                                      [param-key param-value'])))
              ;; No "=" found, use default Boolean true value
              (recur ctx''' (conj (filterv #(not= (first %) param-key) parameters)
                                  [param-key param-value])))))))))
(defn parse-item [ctx]
  ;; RFC 9651 §4.2.3: Parsing an Item
  ;; 1. Let bare_item be the result of running Parsing a Bare Item with input_string.
  (let [[bare-item ctx'] (parse-bare-item ctx)
        ;; 2. Let parameters be the result of running Parsing Parameters with input_string.
        [parameters ctx''] (parse-parameters ctx')]
    ;; 3. Return the tuple (bare_item, parameters).
    [{:type :item :bare bare-item :params parameters} ctx'']))
(defn parse-inner-list [ctx]
  (let [ch (peek-char ctx)]
    (when-not (= ch \()
      (parse-error ctx "Expected opening '(' for inner list" :found ch :expected "("))
    (let [[_ ctx'] (consume-char ctx)
          ctx'' (skip-sp ctx')]
      (loop [ctx'' ctx'' inner-list []]
        (if (eof? ctx'')
          (parse-error ctx'' "Unterminated inner list")
          (let [ch (peek-char ctx'')]
            (if (= ch \))
              (let [[_ ctx'''] (consume-char ctx'')
                    ctx'''' (skip-sp ctx''')
                    [parameters ctx'''''] (parse-parameters ctx'''')]
                [{:type :inner-list :items inner-list :params parameters} ctx'''''])
              (let [[item ctx'''] (parse-item ctx'')
                    inner-list' (conj inner-list item)
                    ctx'''' (skip-sp ctx''')]
                (recur ctx'''' inner-list')))))))))
(defn parse-item-or-inner-list [ctx]
  ;; RFC 9651 §4.2.1.1: Parsing an Item or Inner List
  ;; 1. If the first character is "(", return parse-inner-list
  (let [ch (peek-char ctx)]
    (if (= ch \()
      (parse-inner-list ctx)
      ;; 2. Otherwise, return parse-item
      (parse-item ctx))))
(defn parse-list-members [ctx]
  ;; Parse comma-separated list members
  (loop [ctx ctx members []]
    (let [ctx' (skip-ows ctx)]
      (if (eof? ctx')
        [members ctx']
        (let [[member ctx''] (parse-item-or-inner-list ctx')
              members' (conj members member)
              ctx''' (skip-ows ctx'')]
          (if (eof? ctx''')
            [members' ctx''']
            (let [ch (peek-char ctx''')]
              (if (= ch \,)
                (let [[_ ctx''''] (consume-char ctx''')]
                  (recur ctx'''' members'))
                [members' ctx''']))))))))
(defn parse-list [s-or-bytes]
  ;; RFC 9651 §4.2.1: Parsing a List
  (let [ctx (init-ctx s-or-bytes)
        ctx' (skip-sp ctx)
        [members ctx''] (parse-list-members ctx')
        ctx''' (skip-sp ctx'')]
    (when-not (eof? ctx''')
      (parse-error ctx''' "Unexpected characters after list"))
    {:type :list :members members}))

(defn parse-dict [s-or-bytes]
  ;; RFC 9651 §4.2.2: Parsing a Dictionary
  (let [ctx (init-ctx s-or-bytes)
        ctx' (skip-sp ctx)]
    (loop [ctx ctx' entries []]
      (let [ctx (skip-ows ctx)]
        (if (eof? ctx)
          {:type :dict :entries entries}
          (let [[key ctx] (parse-key ctx)
                ch (peek-char ctx)]
            (if (= ch \=)
              ;; Key=Value
              (let [[_ ctx] (consume-char ctx)
                    [member ctx] (parse-item-or-inner-list ctx)
                    entries' (conj (filterv #(not= (first %) key) entries) [key member])
                    ctx (skip-ows ctx)]
                (if (eof? ctx)
                  {:type :dict :entries entries'}
                  (let [ch2 (peek-char ctx)]
                    (if (= ch2 \,)
                      (let [[_ ctx] (consume-char ctx)]
                        (recur ctx entries'))
                      (parse-error ctx "Expected comma or end of input" :found ch2 :expected ",")))))
              ;; Key only - RFC says this is an Item with Boolean true and parameters
              (let [[params ctx] (parse-parameters ctx)
                    boolean-item {:type :item :bare {:type :boolean :value true} :params params}
                    entries' (conj (filterv #(not= (first %) key) entries) [key boolean-item])
                    ctx (skip-ows ctx)]
                (if (eof? ctx)
                  {:type :dict :entries entries'}
                  (let [ch2 (peek-char ctx)]
                    (if (= ch2 \,)
                      (let [[_ ctx] (consume-char ctx)]
                        (recur ctx entries'))
                      (parse-error ctx "Expected comma or end of input" :found ch2 :expected ","))))))))))))
(defn parse-dictionary [s-or-bytes]
  (parse-dict s-or-bytes))
(defn parse [field-type s-or-bytes]
  ;; RFC 9651 §4.2: Parsing Structured Fields
  (case field-type
    "list" (parse-list s-or-bytes)
    "dictionary" (parse-dict s-or-bytes)
    "item" (let [ctx (init-ctx s-or-bytes)
                 ctx' (skip-sp ctx)
                 [item ctx''] (parse-item ctx')
                 ctx''' (skip-sp ctx'')]
             (when-not (eof? ctx''')
               (parse-error ctx''' "Unexpected characters after item"))
             item)
    (parse-error {:i 0} (str "Unknown field type: " field-type))))

;; Serialization stubs (to be implemented)
(defn serialize [_x] (parse-error {:i 0} "not implemented"))
(defn serialize-list [_l] (parse-error {:i 0} "not implemented"))
(defn serialize-dict [_d] (parse-error {:i 0} "not implemented"))
(defn serialize-item [_i] (parse-error {:i 0} "not implemented"))

;; Utilities
(defn combine-field-lines [lines] (str/join ", " lines))

(defn ascii-bytes [s] (.getBytes ^String s StandardCharsets/US_ASCII))
(defn ordered? [x] (or (vector? x) (sequential? x)))
