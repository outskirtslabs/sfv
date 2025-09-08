;; Copyright © 2025 Casey Link <casey@outskirtslabs.com>
;; SPDX-License-Identifier: MIT
(ns user
  (:require [ol.sfv :as sfv]))

;; Integer
(sfv/parse-item "42")
{:type :item :bare {:type :integer :value 42} :params []}

;; They are round trippable
(sfv/serialize {:type :item :bare {:type :integer :value 42}})
;; => "42"

;; Display String
(sfv/parse-item "%\"Gr%c3%bc%c3%9fe\"")
{:type :item :bare {:type :dstring :value "Grüße"} :params []}

(sfv/serialize {:type :item :bare {:type :dstring :value "السلام عليكم"} :params []})
"%\"%d8%a7%d9%84%d8%b3%d9%84%d8%a7%d9%85 %d8%b9%d9%84%d9%8a%d9%83%d9%85\""

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