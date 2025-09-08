# `ol.sfv`

> A 0-dependency Clojure library for parsing and generating Structured Field Values for HTTP (RFC 9651/8941)

[![Build Status](https://github.com/outskirtslabs/sfv/actions/workflows/ci.yml/badge.svg)](https://github.com/outskirtslabs/sfv/actions)
[![cljdoc badge](https://cljdoc.org/badge/com.outskirtslabs/sfv)](https://cljdoc.org/d/com.outskirtslabs/sfv)
[![Clojars Project](https://img.shields.io/clojars/v/com.outskirtslabs/sfv.svg)](https://clojars.org/com.outskirtslabs/sfv)

Structured Field Values (SFV) as defined in [RFC 9651][rfc9651] provide a standardized way to encode complex data structures in HTTP headers.

`ol.sfv` library implements the specification, providing parsing and serialization capabilities.

This is a low-level library that emits and consumes the RFC 9651 AST, which unfortunately does not map cleanly to Clojure datastructures. 
For practical use, it should be wrapped in higher-level functions or libraries that implement specific HTTP headers like `Permissions-Policy`, `Signature-Input`, or `Signature` headers (any any future HTTP headers).

Key features:

* Complete RFC 9651 implementation with AST-level parsing and serialization with zero deps
* Extensively tested (`2853 tests, 3220 assertions`)
* Precise round-trip fidelity - parse and serialize back to identical strings
* Validation and error reporting
* JVM/Graal target

## Installation

```clojure
{:deps {com.outskirtslabs/sfv {:mvn/version "0.1.0"}}}

;; Leiningen
[com.outskirtslabs/sfv "0.1.0"]
```

## Quick Start

```clojure
(ns myapp.core
  (:require [ol.sfv :as sfv]))

;; Integer
(sfv/parse-item "42")
{:type :item :bare {:type :integer :value 42} :params []}

;; They are round trippable
(sfv/serialize-item {:type :item :bare {:type :integer :value 42}})
;; => "42"

;; Display String
(sfv/parse-item "%\"Gr%c3%bc%c3%9fe\"")
{:type :item :bare {:type :dstring :value "Grüße"} :params []}

(sfv/serialize-item {:type :item :bare {:type :dstring :value "السلام عليكم"} :params []})
"\"%d8%a7%d9%84%d8%b3%d9%84%d8%a7%d9%85 %d8%b9%d9%84%d9%8a%d9%83%d9%85\""

;; Dates
(sfv/parse-item "@1659578233")
{:type :item, :bare {:type :date, :value 1659578233}, :params []}

;; Items can have params attached
(sfv/parse-item "pear;sweet=?1")
{:type   :item
 :bare   {:type :token :value "pear"}
 :params [["sweet" {:type :token :value true}]]}

;; Parse a list with parameters
(sfv/parse-list "apple, pear;sweet=true, orange")
{:type    :list
 :members [{:type :item :bare {:type :token :value "apple"} :params []}
           {:type :item :bare {:type :token :value "pear"} :params [["sweet" {:type :token :value "true"}]]}
           {:type :item :bare {:type :token :value "orange"} :params []}]}

;; Dictionaries
(sfv/parse-dict "max-age=3600, must-revalidate")
{:type    :dict
 :entries [["max-age" {:type :item :bare {:type :integer :value 3600} :params []}]
           ["must-revalidate" {:type :item :bare {:type :boolean :value true} :params []}]]}

;; Dictionaries with inner lists and params
(sfv/parse-dict "trees=(\"spruce\";type=conifer \"oak\";type=deciduous)")
{:type    :dict
 :entries [["trees"
            {:type   :inner-list
             :items  [{:type   :item :bare {:type :string :value "spruce"}
                       :params [["type" {:type :token :value "conifer"}]]}
                      {:type   :item :bare {:type :string :value "oak"}
                       :params [["type" {:type :token :value "deciduous"}]]}]
             :params []}]]}

(sfv/parse-dict "foods=(\"burger\";sandwich=?1 \"pizza\";sandwich=?0 \"hot dog\";sandwich=?1);comprehensive=?0")
{:type    :dict
 :entries [["foods" {:type   :inner-list
                     :items  [{:type   :item :bare {:type :string :value "burger"}
                               :params [["sandwich" {:type :boolean :value true}]]}
                              {:type   :item :bare {:type :string :value "pizza"}
                               :params [["sandwich" {:type :boolean :value false}]]}
                              {:type   :item :bare {:type :string :value "hot dog"}
                               :params [["sandwich" {:type :boolean :value true}]]}]
                     :params [["comprehensive" {:type :boolean :value false}]]}]]}

;; List with Items and Inner List
(sfv/parse-list "circle;color=red, square;filled=?0, (triangle;size=3 rectangle;size=4)")
{:type    :list
 :members [{:type   :item :bare {:type :token :value "circle"}
            :params [["color" {:type :token :value "red"}]]}
           {:type   :item :bare {:type :token :value "square"}
            :params [["filled" {:type :boolean :value false}]]}
           {:type   :inner-list :items [{:type   :item :bare {:type :token :value "triangle"}
                                         :params [["size" {:type :integer :value 3}]]}
                                        {:type   :item :bare {:type :token :value "rectangle"}
                                         :params [["size" {:type :integer :value 4}]]}]
            :params []}]}
```

## Data Types

`ol.sfv` is a low-level, AST-oriented implementation of RFC 9651.
We don't try to coerce values into "nice" Clojure shapes; instead we expose a precise tree that round-trips byte-for-byte.
This is important because Structured Fields carry ordering information that ordinary Clojure maps can't reliably preserve across platforms and sizes.

### Primitive types

RFC 9651 defines several primitive types, all supported.

Each Item's `:bare` is one of the following:

| SFV type       | Header                         | AST example                              | Clojure type (`:value`)               |
|----------------|--------------------------------|------------------------------------------|---------------------------------------|
| Integer        | `42`, `-17`, `999999999999999` | `{:type :integer :value 1618884473}`     | `long`                                |
| Decimal        | `3.14`, `-0.5`                 | `{:type :decimal :value 3.14M}`          | `BigDecimal`                          |
| String         | `"hello world"`                | `{:type :string :value "hello"}`         | `java.lang.String`                    |
| Token          | `simple-token`                 | `{:type :token :value "simple-token"}`   | `String`                              |
| Byte Sequence  | `:SGVsbG8=:`                   | `{:type :bytes :value <platform bytes>}` | `byte[]`                              |
| Boolean        | `?1` / `?0`                    | `{:type :boolean :value true}`           | `true` / `false`                      |
| Date           | `@1659578233`                  | `{:type :date :value 1659578233}`        | epoch seconds as `long`               |
| Display String | `%"Gr%c3%bc%c3%9fe"`           | `{:type :display :value "Grüße"}`        | `String` (percent-decoded, validated) |

- Decimals obey SFV's constraints (≤3 fractional digits, length limits) and are parsed to `BigDecimal` to avoid float rounding.
- Byte sequences are base64 inside the header; we give you the decoded bytes.


### Container types

- **Item** — a **bare value** plus optional **parameters**
  ```clojure
  {:type :item
   :bare   <bare-ast>
   :params [ [param-name <bare-ast-or-true>] ... ]}
  ```

- **List** — a sequence of **items** or **inner lists**
  ```clojure
  {:type :list
   :members [ <item-or-inner-list> ... ]}
  ```

- **Dictionary** — an **ordered** sequence of key→member entries
  ```clojure
  {:type :dict
   :entries [ [key <item-or-inner-list>] ... ]}
  ```

- **Inner List** — a parenthesized list (appears inside List/Dictionary) with its own parameters
  ```clojure
  {:type :inner-list
   :items  [ <item> ... ]
   :params [ [param-name <bare-ast-or-true>] ... ]}
  ```

Keys are parsed as lower-case identifiers per the spec.


### Why not plain Clojure maps?

Dictionaries and parameter lists in SFV have a defined member order.
That order may or may not be semantically meaningful, that depends on the specific header.

To make ordering explicit and stable, we represent dictionaries and parameter lists as vectors of [k v] pairs. 
That's portable, preserves order, and lets you implement whatever key semantics you need at a higher level.

### Parameters

Parameters attach metadata to an Item or an Inner List:

- Syntax: `;key[=value]` repeating after the base value
  If `=value` is omitted, the parameter value is the boolean `true`.

- In the AST, parameters are always a vector of `[name value]` pairs in the order seen:
  ```clojure
  (sfv/parse-item "pear;sweet=?1")
  {:type :item
  :bare   {:type :token :value "pear"}
  :params [["sweet" {:type :boolean :value true}]]}
  ```

- Parameter values are bare values (not nested items). You'll see the same `:type`/`:value` shapes as the table above
- Ordering is preserved for round-trip fidelity


### Dictionaries

Dictionaries map from a key to either an Item or an Inner List. We keep them as an ordered vector of entries:

```clojure
(sfv/parse-dict "max-age=3600, must-revalidate")
{:type    :dict
 :entries [["max-age" {:type :item :bare {:type :integer :value 3600} :params []}]
           ["must-revalidate" {:type :item :bare {:type :boolean :value true} :params []}]]}
```


## Building Header-Specific Libraries

This library is designed to be wrapped by more specific implementations:

```clojure
(ns myapp.cache-control
  (:require [ol.sfv :as sfv]))

(defn parse-cache-control [header-value]
  (let [parsed (sfv/parse-dict header-value)]
    (reduce (fn [acc [key item]]
              (let [value (get-in item [:bare :value])]
                (assoc acc (keyword key) value)))
            {}
            (:entries parsed))))

(parse-cache-control "max-age=3600, must-revalidate")
;; => {:max-age 3600, :must-revalidate true}
```

## Recommended Reading

Structured Field Values provide a robust foundation for modern HTTP header design:

* [RFC 9651: Structured Field Values for HTTP][rfc9651] - The complete specification
* [RFC 9421: HTTP Message Signatures][rfc9421] - A major consumer of structured fields

## Security

See [here][sec] for security advisories or to report a security vulnerability.

## License

Copyright © 2025 Casey Link <casey@outskirtslabs.com>

Distributed under the [MIT License](./LICENSE)

[sec]: https://github.com/outskirtslabs/sfv/security
[rfc9651]: https://datatracker.ietf.org/doc/html/rfc9651
[rfc9421]: https://datatracker.ietf.org/doc/html/rfc9421
