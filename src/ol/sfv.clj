(ns ol.sfv
  "RFC 9651 Structured Field Values for HTTP.

  See the project website for info on motivation and design:
  <https://github.com/outskirtslabs/sfv>

  This ns provides parsing and serialization of Structured Fields containing
  Items, Lists, and Dictionaries with Parameters. Returns precise AST
  representations that round-trip byte-for-byte with RFC 9651 strings.

  Primary functions: [[parse]], [[parse-item]], [[parse-list]], [[parse-dict]],
  [[serialize]]

  ## Conventions

  - `s-or-bytes`: Input string or byte array (decoded as ascii) containing HTTP field value
  - `field-type`: One of `:item`, `:list`, or `:dict` specifying the top-level structure
  - `:bare`: The raw value within an Item (Integer, Decimal, String, Token, Byte Sequence, Boolean, Date, or Display String)

  ## Primitive Types

  Each Item's `:bare` is one of these RFC 9651 types:

  | SFV type       | Header                         | AST example                                | Clojure type (`:value`)               |
  |----------------|--------------------------------|--------------------------------------------|---------------------------------------|
  | Integer        | `42`, `-17`, `999999999999999` | `{:type :integer :value 1618884473}`       | `long`                                |
  | Decimal        | `3.14`, `-0.5`                 | `{:type :decimal :value 3.14M}`            | `BigDecimal`                          |
  | String         | `\" hello world \"`            | `{:type :string :value \" hello \"}`       | `java.lang.String`                    |
  | Token          | `simple-token`                 | `{:type :token :value \" simple-token \"}` | `String`                              |
  | Byte Sequence  | `:SGVsbG8=:`                   | `{:type :bytes :value <platform bytes>}`   | `byte[]`                              |
  | Boolean        | `?1` / `?0`                    | `{:type :boolean :value true}`             | `true` / `false`                      |
  | Date           | `@1659578233`                  | `{:type :date :value 1659578233}`          | epoch seconds as `long`               |
  | Display String | `%\" Gr%c3%bc%c3%9fe \"`       | `{:type :display :value \" Grüße \"}`      | `String` (percent-decoded, validated) |
  "

  (:refer-clojure :exclude [list integer? decimal? string? bytes bytes?])
  (:require [ol.sfv.impl :as impl]))

(defn parse
  "Parse a Structured Field of the given `field-type`.

  Takes a `field-type` (`:list`, `:dict`, or `:item`) and a string or byte array.
  Returns an AST representation following RFC 9651.

  ```clojure
  (parse :item \"42\")
  ;; => {:type :item :bare {:type :integer :value 42} :params []}

  (parse :dict \"max-age=3600, must-revalidate\")
  ;; => {:type :dict :entries [[\"max-age\" {...}] [\"must-revalidate\" {...}]]}
  ```"
  [field-type s-or-bytes]
  (impl/parse field-type s-or-bytes))

(defn parse-list
  "Parse a Structured Field List from `s-or-bytes`.

  Returns a List AST with `:type :list` and `:members` vector containing Items and Inner Lists.

  ```clojure
  (parse-list \"apple, pear;sweet=true, orange\")
  ;; => {:type :list :members [...]}
  ```"
  [s-or-bytes]
  (impl/parse-list s-or-bytes))

(defn parse-dict
  "Parse a Structured Field Dictionary from `s-or-bytes`.

  Returns a Dictionary AST with `:type :dict` and `:entries` as ordered key-value pairs.
  Each entry maps from a key to either an Item or Inner List.

  ```clojure
  (parse-dict \"max-age=3600, must-revalidate\")
  ;; => {:type :dict :entries [[\"max-age\" {...}] [\"must-revalidate\" {...}]]}
  ```"
  [s-or-bytes]
  (impl/parse-dict s-or-bytes))

(defn parse-item
  "Parse a Structured Field Item from `s-or-bytes`.

  Returns an Item AST with `:type :item`, `:bare` value, and `:params` Parameters.
  The bare value can be Integer, Decimal, String, Token, Byte Sequence, Boolean, Date, or Display String.

  ```clojure
  (parse-item \"42\")
  ;; => {:type :item :bare {:type :integer :value 42} :params []}

  (parse-item \"pear;sweet\")
  ;; => {:type :item :bare {:type :token :value \"pear\"} :params [[\"sweet\" {...}]]}
  ```"
  [s-or-bytes]
  (first (impl/parse-item (impl/init-ctx s-or-bytes))))

(defn serialize
  "Serialize a Structured Field AST `x` to its string representation.

  Takes any parsed AST (Item, List, or Dictionary) and returns the RFC 9651 string.
  See [[serialize-item]], [[serialize-list]], and [[serialize-dict]] for type-specific variants.

  ```clojure
  (serialize {:type :item :bare {:type :integer :value 42} :params []})
  ;; => \"42\"

  (serialize-list {:type :list :members [...]})
  ;; => \"apple, pear;sweet=true, orange\"

  (serialize-dict {:type :dict :entries [[\"max-age\" {...}] [\"must-revalidate\" {...}]]})
  ;; => \"max-age=3600, must-revalidate\"
  ```"
  [x]
  (impl/serialize x))

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
