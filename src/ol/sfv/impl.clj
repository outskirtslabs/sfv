(ns ol.sfv.impl
  (:require [clojure.string :as str])
  (:import (java.nio.charset StandardCharsets)))

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
(defn bytes? [x] (or (instance? (Class/forName "[B") x)
                     (instance? java.nio.ByteBuffer x)))

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

(defn inner-list
  ([items] {:type :inner-list :items items :params []})
  ([items ps] {:type :inner-list :items items :params ps}))

(defn sf-list [members] {:type :list :members members})
(defn sf-dict [entries] {:type :dict :entries entries})

(defn flag
  ([] {:type :flag :params []})
  ([ps] {:type :flag :params ps}))

;; ---------------------------------------------------------------------------
;; Scanner / cursor utilities

(defn ascii-string [s]
  (cond
    (string? s) s
    (instance? (Class/forName "[B") s) (String. ^bytes s StandardCharsets/US_ASCII)
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

(defn parse-error [ctx reason & {:keys [found expected]}]
  (throw (ex-info reason (merge {:ol.sfv/error true :pos (:i ctx)}
                                 (when found {:found found})
                                 (when expected {:expected expected})))) )

;; ---------------------------------------------------------------------------
;; parse-key (RFC 9651 §4.2.3.3)

(defn parse-key [ctx]
  (let [ch (peek-char ctx)]
    (when-not (and ch (or (lcalpha? ch) (= ch \*)))
      (parse-error ctx "Invalid key start" :found ch :expected "lcalpha or '*'") )
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

                  :else
                  (let [input-number (.toString sb) ctx' (assoc ctx :i i)]
                    (if (= type :integer)
                      (let [num (Long/parseLong input-number)] [{:type :integer :value (* sign num)} ctx'])
                      (do (when (.endsWith input-number ".") (parse-error ctx "Decimal has trailing dot"))
                          (let [dot-pos (.indexOf input-number \\.)
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
                      (let [dot-pos (.indexOf input-number \\.)]
                        (if (= dot-pos -1)
                          (let [num (Long/parseLong input-number)] [{:type :integer :value (* sign num)} ctx'])
                          (let [frac-len (- (.length input-number) (inc dot-pos))]
                            (when (> frac-len 3) (parse-error ctx "Decimal fraction too long"))
                            (let [bd (java.math.BigDecimal. input-number)
                                  bd (if (= sign -1) (.negate bd) bd)]
                              [{:type :decimal :value bd} ctx']))))))))))))))

;; The remaining parsers (parse-string, parse-token, parse-byte-sequence,
;; parse-boolean, parse-date, parse-display-string, parse-parameters,
;; item/inner-list/list/dictionary) will be implemented next, following
;; the exact step-by-step algorithms in RFC 9651 (Section 4.2 and 4.1).

;; Stub other parsers for now (kept until implemented):
(defn parse-string [ctx] (parse-error ctx "not implemented"))
(defn parse-token [ctx] (parse-error ctx "not implemented"))
(defn parse-byte-sequence [ctx] (parse-error ctx "not implemented"))
(defn parse-boolean [ctx] (parse-error ctx "not implemented"))
(defn parse-date [ctx] (parse-error ctx "not implemented"))
(defn parse-display-string [ctx] (parse-error ctx "not implemented"))
(defn parse-bare-item [ctx] (parse-error ctx "not implemented"))
(defn parse-parameters [ctx] (parse-error ctx "not implemented"))
(defn parse-item [ctx] (parse-error ctx "not implemented"))
(defn parse-inner-list [ctx] (parse-error ctx "not implemented"))
(defn parse-item-or-inner-list [ctx] (parse-error ctx "not implemented"))
(defn parse-list-members [ctx] (parse-error ctx "not implemented"))
(defn parse-list [s-or-bytes] (parse-error {:i 0} "not implemented"))
(defn parse-dictionary [s-or-bytes] (parse-error {:i 0} "not implemented"))
(defn parse [field-type s-or-bytes] (parse-error {:i 0} "not implemented"))

;; Serialization stubs (to be implemented)
(defn serialize [x] (parse-error {:i 0} "not implemented"))
(defn serialize-list [l] (parse-error {:i 0} "not implemented"))
(defn serialize-dict [d] (parse-error {:i 0} "not implemented"))
(defn serialize-item [i] (parse-error {:i 0} "not implemented"))

;; Utilities
(defn combine-field-lines [lines] (str/join ", " lines))
(defn ordered? [x] (or (vector? x) (sequential? x)))
