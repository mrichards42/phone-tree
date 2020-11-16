(ns phone-tree.parse-op-type-test
  "Tests for parse/op-type"
  (:require [clojure.test :refer [deftest testing is are]]
            [phone-tree.parse :as parse]
            [phone-tree.test-util :as t.util]
            [clojure.tools.analyzer.ast :as ana.ast]))

(defn ops-with-name
  "Finds all ops in a form (recursively), and returns a seq of [:op :name]."
  [form]
  (->> form
       t.util/analyze+eval
       ana.ast/nodes
       (map (juxt parse/op-type (some-fn ;; methods, vars
                                         :name
                                         (comp :name meta :var)
                                         (comp :name meta :var :protocol-fn)
                                         :method)))
       (filter identity)))

(defn ops-without-name
  "Finds all ops in a form (recursively)"
  [form]
  (map first (ops-with-name form)))

(deftest op-type-def-test
  (testing "Understands def"
    (is (some '#{[:def a-const]} (ops-with-name '(def a-const 10)))
        "in def form")
    (is (some '#{[:def a-fn]} (ops-with-name '(defn a-fn [x] (* 10 x))))
        "in defn form")))

(deftest op-type-var-test
  (testing "Understands var"
    (t.util/analyze+eval '(def a-var 10))
    (is (some '#{[:var a-var]} (ops-with-name 'a-var))
        "as a symbol")
    (is (some '#{[:the-var a-var]} (ops-with-name '#'a-var))
        "as a var")))

(deftest op-type-multimethod-test
  (testing "Understands defmulti/defmethod"
    (is (some '#{[:def a-multi]} (ops-with-name '(defmulti a-multi identity)))
        "in defmulti")
    ;; defmethod is a function call, so there isn't a var name
    (is (some #{:defmethod}
              (ops-without-name '(defmethod a-multi :hi [_])))
        "in defmethod")))

(deftest op-type-protocol-test
  (testing "Understands defprotocol"
    ;; defprotocol is a macro, so there isn't a var name
    (is (= 2 (->> (ops-without-name '(do
                                       (defprotocol AProtocol
                                         (a-method [this y])
                                         (another-method [this]))

                                       (defprotocol BProtocol
                                         (a-third-method [this]))))
                  (filter #{:defprotocol})
                  count))))

  ;; These tests rely on the above protocols
  (testing "Understands deftype methods"
    (is (every? (set (ops-with-name '(deftype a-type []
                                     AProtocol
                                     (a-method [this y])
                                     (another-method [this]))))
                '#{[:def ->a-type]
                   [:method a-method]
                   [:method another-method]})))

  (testing "Understands defrecord methods"
    (is (every? (set (ops-with-name '(defrecord a-record [a b]
                                     AProtocol
                                     (a-method [this y])
                                     (another-method [this]))))
                ;; defrecord creates a bunch of extra methods that we don't
                ;; care about so that it satisfies the map protocol
                '#{[:def ->a-record]
                   [:def map->a-record]
                   [:method a-method]
                   [:method another-method]})))

  (testing "Understands reify methods"
    (is (every? (set (ops-with-name '(reify AProtocol
                                     (a-method [this y])
                                     (another-method [this]))))
                '#{[:method a-method]
                   [:method another-method]})))

  (testing "Understands extend"
    ;; extend-type only reports a single :extend operation; all
    ;; names and methods are handled in parse-one*
    ;; extend is a function call, so there isn't a var name
    (is (= 1 (->> (ops-without-name '(extend-type java.lang.String
                                       AProtocol
                                       (a-method [this y])
                                       (another-method [this])
                                       BProtocol
                                       (a-third-method [this])))
                  (filter #{:extend})
                  count))
        "in extend-type")

    ;; extend-protocol needs an :extend call per type
    ;; methods are handled in parse-one*
    ;; extend is a function call, so there isn't a var name
    (is (= 2 (->> (ops-without-name '(extend-protocol AProtocol
                                       java.lang.Number
                                       (a-method [this y])
                                       (another-method [this])

                                       java.lang.Object
                                       (a-method [this y])
                                       (another-method [this])))
                  (filter #{:extend})
                  count))
          "in extend-protocol"))

  (testing "Understands invocations"
    (is (every? (set (ops-with-name '(a-method (->a-type) 1)))
                '#{[:protocol-invoke a-method]
                   [:var ->a-type]})
        "of a protocol method")
    (is (every? (set (ops-with-name '(.a-method (->a-type) 1)))
                '#{[:instance-call a-method]
                   [:var ->a-type]})
        "of an instance method")
    (is (some #{:new} (ops-without-name '(a-type.)))
        "of a constructor")
    (is (some #{:static-call} (ops-without-name '(System/getenv "HI")))
        "of a static method")))
