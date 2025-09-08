(ns ol.sfv.example-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.walk :as walk]
            [ol.sfv :as sfv]))

(defn trim-bytes
  "Replace byte arrays with truncated vector forms to keep examples readable"
  [data]
  (walk/postwalk #(if (bytes? %) (vec (take 5 (seq %))) %) data))

(deftest user-examples
  ;; Integer
  (is (= {:type :item :bare {:type :integer :value 42} :params []}
         (sfv/parse-item "42")))
  (is (= "42" (sfv/serialize (sfv/parse-item "42"))))

  ;; Display String
  (is (= {:type :item :bare {:type :dstring :value "Grüße"} :params []}
         (sfv/parse-item "%\"Gr%c3%bc%c3%9fe\"")))
  (is (= "%\"Gr%c3%bc%c3%9fe\"" (sfv/serialize (sfv/parse-item "%\"Gr%c3%bc%c3%9fe\""))))

  (is (=
       "%\"%d8%a7%d9%84%d8%b3%d9%84%d8%a7%d9%85 %d8%b9%d9%84%d9%8a%d9%83%d9%85\""
       (sfv/serialize {:type :item :bare {:type :dstring :value "السلام عليكم"} :params []})))
  (is (= "%\"%d8%a7%d9%84%d8%b3%d9%84%d8%a7%d9%85 %d8%b9%d9%84%d9%8a%d9%83%d9%85\""
         (sfv/serialize (sfv/parse-item "%\"%d8%a7%d9%84%d8%b3%d9%84%d8%a7%d9%85 %d8%b9%d9%84%d9%8a%d9%83%d9%85\""))))

  ;; Dates
  (is (= {:type :item, :bare {:type :date, :value 1659578233}, :params []}
         (sfv/parse-item "@1659578233")))
  (is (= "@1659578233" (sfv/serialize (sfv/parse-item "@1659578233"))))

;; Items can have params attached
  (is (= {:type   :item
          :bare   {:type :token :value "pear"}
          :params [["sweet" {:type :boolean :value true}]]}
         (sfv/parse-item "pear;sweet")))
  (is (= "pear;sweet" (sfv/serialize (sfv/parse-item "pear;sweet"))))

  ;; Parse a list with parameters
  (is (= {:type    :list
          :members [{:type :item :bare {:type :token :value "apple"} :params []}
                    {:type :item :bare {:type :token :value "pear"} :params [["sweet" {:type :token :value "true"}]]}
                    {:type :item :bare {:type :token :value "orange"} :params []}]}
         (sfv/parse-list "apple, pear;sweet=true, orange")))
  (is (= "apple, pear;sweet=true, orange" (sfv/serialize (sfv/parse-list "apple, pear;sweet=true, orange"))))

  ;; Parse a dictionary (common in HTTP headers)
  (is (= {:type    :dict
          :entries [["max-age" {:type :item :bare {:type :integer :value 3600} :params []}]
                    ["must-revalidate" {:type :item :bare {:type :boolean :value true} :params []}]]}
         (sfv/parse-dict "max-age=3600, must-revalidate")))
  (is (= "max-age=3600, must-revalidate" (sfv/serialize (sfv/parse-dict "max-age=3600, must-revalidate"))))

  (is (= {:type    :dict
          :entries [["trees"
                     {:type   :inner-list
                      :items  [{:type   :item :bare {:type :string :value "spruce"}
                                :params [["type" {:type :token :value "conifer"}]]}
                               {:type   :item :bare {:type :string :value "oak"}
                                :params [["type" {:type :token :value "deciduous"}]]}]
                      :params []}]]}
         (sfv/parse-dict "trees=(\"spruce\";type=conifer \"oak\";type=deciduous)")))
  (is (= "trees=(\"spruce\";type=conifer \"oak\";type=deciduous)"
         (sfv/serialize (sfv/parse-dict "trees=(\"spruce\";type=conifer \"oak\";type=deciduous)"))))

  (is (= {:type    :dict
          :entries [["foods" {:type   :inner-list
                              :items  [{:type   :item :bare {:type :string :value "burger"}
                                        :params [["sandwich" {:type :boolean :value true}]]}
                                       {:type   :item :bare {:type :string :value "pizza"}
                                        :params [["sandwich" {:type :boolean :value false}]]}
                                       {:type   :item :bare {:type :string :value "hot dog"}
                                        :params [["sandwich" {:type :boolean :value true}]]}]
                              :params [["comprehensive" {:type :boolean :value false}]]}]]}
         (sfv/parse-dict "foods=(\"burger\";sandwich \"pizza\";sandwich=?0 \"hot dog\";sandwich);comprehensive=?0")))
  (is (= "foods=(\"burger\";sandwich \"pizza\";sandwich=?0 \"hot dog\";sandwich);comprehensive=?0"
         (sfv/serialize (sfv/parse-dict "foods=(\"burger\";sandwich \"pizza\";sandwich=?0 \"hot dog\";sandwich);comprehensive=?0"))))

  (is (= {:type    :list
          :members [{:type   :item :bare {:type :token :value "circle"}
                     :params [["color" {:type :token :value "red"}]]}
                    {:type   :item :bare {:type :token :value "square"}
                     :params [["filled" {:type :boolean :value false}]]}
                    {:type   :inner-list :items [{:type   :item :bare {:type :token :value "triangle"}
                                                  :params [["size" {:type :integer :value 3}]]}
                                                 {:type   :item :bare {:type :token :value "rectangle"}
                                                  :params [["size" {:type :integer :value 4}]]}]
                     :params []}]}
         (sfv/parse-list "circle;color=red, square;filled=?0, (triangle;size=3 rectangle;size=4)")))
  (is (= "circle;color=red, square;filled=?0, (triangle;size=3 rectangle;size=4)"
         (sfv/serialize (sfv/parse-list "circle;color=red, square;filled=?0, (triangle;size=3 rectangle;size=4)")))))

(deftest signature-input-examples
  (testing "Parse and serialize Signature-Input headers from RFC 9421"

    (testing "Minimal signature with empty covered components"
      (let [input  "sig-b21=();created=1618884473;keyid=\"test-key-rsa-pss\";nonce=\"b3k2pp5k7z-50gnwp.yemd\""
            parsed (sfv/parse-dict input)]
        (is (= {:type    :dict
                :entries [["sig-b21" {:type   :inner-list
                                      :items  []
                                      :params [["created" {:type :integer :value 1618884473}]
                                               ["keyid" {:type :string :value "test-key-rsa-pss"}]
                                               ["nonce" {:type :string :value "b3k2pp5k7z-50gnwp.yemd"}]]}]]} parsed))
        (is (= input (sfv/serialize parsed)))))

    (testing "Signature with covered components including query parameter"
      (let [input  "sig-b22=(\"@authority\" \"content-digest\" \"@query-param\";name=\"Pet\");created=1618884473;keyid=\"test-key-rsa-pss\";tag=\"header-example\""
            parsed (sfv/parse-dict input)]
        (is (= {:type    :dict
                :entries [["sig-b22" {:type   :inner-list
                                      :items  [{:type   :item
                                                :bare   {:type :string :value "@authority"}
                                                :params []}
                                               {:type   :item
                                                :bare   {:type :string :value "content-digest"}
                                                :params []}
                                               {:type   :item
                                                :bare   {:type :string :value "@query-param"}
                                                :params [["name" {:type :string :value "Pet"}]]}]
                                      :params [["created" {:type :integer :value 1618884473}]
                                               ["keyid" {:type :string :value "test-key-rsa-pss"}]
                                               ["tag" {:type :string :value "header-example"}]]}]]}
               parsed))
        (is (= input (sfv/serialize parsed)))))))

(deftest signature-examples
  (testing "Parse and serialize Signature headers from RFC 9421"

    (testing "RSA-PSS signature shows byte sequence structure"
      (let [input  "sig-b21=:d2pmTvmbncD3xQm8E9ZV2828BjQWGgiwAaw5bAkgibUopemLJcWDy/lkbbHAve4cRAtx31Iq786U7it++wgGxbtRxf8Udx7zFZsckzXaJMkA7ChG52eSkFxykJeNqsrWH5S+oxNFlD4dzVuwe8DhTSja8xxbR/Z2cOGdCbzR72rgFWhzx2VjBqJzsPLMIQKhO4DGezXehhWwE56YCE+O6c0mKZsfxVrogUvA4HELjVKWmAvtl6UnCh8jYzuVG5WSb/QEVPnP5TmcAnLH1g+s++v6d4s8m0gCw1fV5/SITLq9mhho8K3+7EPYTU8IU1bLhdxO5Nyt8C8ssinQ98Xw9Q==:"
            parsed (sfv/parse-dict input)]
        (is (= {:entries [["sig-b21" {:bare {:type :bytes :value [119 106 102 78 -7]} :params [] :type :item}]] :type :dict}
               (trim-bytes parsed)))
        (is (= input (sfv/serialize parsed)))))

    (testing "Multiple signatures show dictionary structure"
      (let [input  "reqres=:dMT/A/76ehrdBTD/2Xx8QuKV6FoyzEP/I9hdzKN8LQJLNgzU4W767HK05rx1i8meNQQgQPgQp8wq2ive3tV5Ag==:, another=:dGVzdDEyMw==:"
            parsed (sfv/parse-dict input)]
        (is (=
             {:entries [["reqres" {:bare {:type :bytes :value [116 -60 -1 3 -2]} :params [] :type :item}]
                        ["another"
                         {:bare {:type :bytes :value [116 101 115 116 49]} :params [] :type :item}]]
              :type    :dict}
             (trim-bytes parsed)))
        (is (= input (sfv/serialize parsed)))))))

(deftest combined-signature-headers-example
  (testing "Complete HTTP message signature example from RFC 9421"
    (let [signature-input "sig1=(\"@method\" \"@authority\" \"@path\" \"content-digest\" \"content-type\" \"content-length\");created=1618884475;keyid=\"test-key-rsa-pss\""
          signature       "sig1=:HIbjHC5rS0BYaa9v4QfD4193TORw7u9edguPh0AW3dMq9WImrlFrCGUDih47vAxi4L2YRZ3XMJc1uOKk/J0ZmZ+wcta4nKIgBkKq0rM9hs3CQyxXGxHLMCy8uqK488o+9jrptQ+xFPHK7a9sRL1IXNaagCNN3ZxJsYapFj+JXbmaI5rtAdSfSvzPuBCh+ARHBmWuNo1UzVVdHXrl8ePL4cccqlazIJdC4QEjrF+Sn4IxBQzTZsL9y9TP5FsZYzHvDqbInkTNigBcE9cKOYNFCn4D/WM7F6TNuZO9EgtzepLWcjTymlHzK7aXq6Am6sfOrpIC49yXjj3ae6HRalVc/g==:"

          parsed-input (sfv/parse-dict signature-input)
          parsed-sig   (sfv/parse-dict signature)]

      (is (= {:type    :dict
              :entries [["sig1" {:type   :inner-list
                                 :items  [{:type :item :bare {:type :string :value "@method"} :params []}
                                          {:type :item :bare {:type :string :value "@authority"} :params []}
                                          {:type :item :bare {:type :string :value "@path"} :params []}
                                          {:type :item :bare {:type :string :value "content-digest"} :params []}
                                          {:type :item :bare {:type :string :value "content-type"} :params []}
                                          {:type :item :bare {:type :string :value "content-length"} :params []}]
                                 :params [["created" {:type :integer :value 1618884475}]
                                          ["keyid" {:type :string :value "test-key-rsa-pss"}]]}]]}
             parsed-input))
      (is (= signature-input (sfv/serialize parsed-input)))
      (is (= signature (sfv/serialize parsed-sig))))))
