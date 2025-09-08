(ns ol.sfv
  (:refer-clojure :exclude [list integer? decimal? string? bytes bytes?])
  (:require [ol.sfv.impl :as impl]))

;; Bare value constructors and predicates
(defn token [s]
  (impl/token s))

(defn token? [x]
  (impl/token? x))

(defn decimal [x]
  (impl/decimal x))
(defn decimal? [x]
  (impl/decimal? x))

(defn integer [n]
  (impl/integer n))

(defn integer? [x]
  (impl/integer? x))

(defn string [s]
  (impl/string s))

(defn string? [x]
  (impl/string? x))

(defn dstring [s]
  (impl/dstring s))

(defn dstring? [x]
  (impl/dstring? x))

(defn bytes [b]
  (impl/bytes b))

(defn bytes? [x]
  (impl/bytes? x))

(defn bool [b]
  (impl/bool b))

(defn bool? [x]
  (impl/bool? x))

(defn date [seconds]
  (impl/date seconds))

(defn date? [x]
  (impl/date? x))

;; Parameters (ordered key->bare mappings)
(defn params [& kvs]
  (apply impl/params kvs))

(defn param-get [ps k]
  (impl/param-get ps k))

(defn param-keys [ps]
  (impl/param-keys ps))

;; Items
(defn item
  ([bare]
   (impl/item bare))
  ([bare ps]
   (impl/item bare ps)))

(defn item? [x]
  (impl/item? x))

(defn item-bare [i]
  (impl/item-bare i))

(defn item-params [i]
  (impl/item-params i))

;; Inner Lists
(defn inner-list
  ([items]
   (impl/inner-list items))
  ([items ps]
   (impl/inner-list items ps)))

(defn inner-list? [x]
  (impl/inner-list? x))

(defn inner-items [il]
  (impl/inner-items il))

(defn inner-params [il]
  (impl/inner-params il))

;; Lists
(defn sf-list [members]
  (impl/sf-list members))

(defn sf-list? [x]
  (impl/sf-list? x))

(defn list-members [l]
  (impl/list-members l))

;; Dictionaries
(defn sf-dict [entries]
  (impl/sf-dict entries))

(defn sf-dict? [x]
  (impl/sf-dict? x))

(defn dict-keys [d]
  (impl/dict-keys d))

(defn dict-get [d k]
  (impl/dict-get d k))

(defn dict->pairs [d]
  (impl/dict->pairs d))

;; Dictionary "flag" member (Boolean true with optional params)
(defn flag
  ([]
   (impl/flag))
  ([ps]
   (impl/flag ps)))

(defn flag? [x]
  (impl/flag? x))

;; Parsing
(defn parse [field-type s-or-bytes]
  (impl/parse field-type s-or-bytes))

(defn parse-list [s-or-bytes]
  (impl/parse-list s-or-bytes))

(defn parse-dict [s-or-bytes]
  (impl/parse-dict s-or-bytes))

(defn parse-item [s-or-bytes]
  (first (impl/parse-item (impl/init-ctx s-or-bytes))))

;; Serialization
(defn serialize [x]
  (impl/serialize x))

(defn serialize-list [l]
  (impl/serialize-list l))

(defn serialize-dict [d]
  (impl/serialize-dict d))

(defn serialize-item [i]
  (impl/serialize-item i))

;; Utilities
(defn combine-field-lines [lines]
  (impl/combine-field-lines lines))
(defn ascii-bytes [s]
  (impl/ascii-bytes s))
(defn ascii-string [bytes]
  (impl/ascii-string bytes))
(defn ordered? [x]
  (impl/ordered? x))
