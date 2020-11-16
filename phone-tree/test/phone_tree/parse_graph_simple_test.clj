(ns phone-tree.parse-graph-simple-test
  "Tests for parse/ast->graph with simple forms (everything except interfaces,
  protocols, records, and types)"
  (:require [clojure.test :refer [deftest testing is are]]
            [phone-tree.parse :as parse]
            [phone-tree.test-util :as t.util]))

(defn simple-graph
  [form]
  (->> form t.util/analyze+eval parse/ast->graph t.util/simplify-graph))

(deftest ast->graph-def-test
  (testing "simple def"
    (is (= '{:nodes #{{:symbol test-ns/x}}
             :edges #{}}
           (simple-graph '(def x 10)))))
  (testing "def with calls to core fns"
    (is (= '{:nodes #{{:symbol test-ns/y}}
             :edges #{{:caller {:symbol test-ns/y} :call {:symbol clojure.core/map}}
                      {:caller {:symbol test-ns/y} :call {:symbol clojure.core/inc}}}}
           (simple-graph '(def y (map inc [10 20]))))))
  (testing "defn with recursion"
    (is (= '{:nodes #{{:symbol test-ns/fib}}
             :edges #{{:caller {:symbol test-ns/fib}
                       :call {:symbol test-ns/fib}}
                      ;; inlined math functions
                      {:caller {:symbol test-ns/fib}
                       :call {:symbol clojure.lang.Numbers/add}}
                      {:caller {:symbol test-ns/fib}
                       :call {:symbol clojure.lang.Numbers/minus}}}}
           (simple-graph '(defn fib [x]
                            (case x
                              0 0
                              1 1
                              (+ (fib (- x 2))
                                 (fib (- x 1))))))))))

(deftest ast->graph-multimethods-test
  (testing "defmulti"
    (let [graph (->> '(clojure.core/defmulti b-multi identity)
                     t.util/analyze+eval
                     parse/ast->graph
                     t.util/simplify-graph)]
      (is (contains? (:nodes graph)
                     '{:symbol test-ns/b-multi})
          "multimethod is defined")
      (is (contains? (:edges graph)
                     '{:caller {:symbol test-ns/b-multi} :call {:symbol clojure.core/identity}})
          "call from multimethod to dispatch exists")))

  (testing "defmethod"
    (let [graph (->> '(do
                        (defmulti c-multi identity)
                        (defmethod c-multi :hi [_]
                          (println "Hello world")))
                     t.util/analyze+eval
                     parse/ast->graph
                     t.util/simplify-graph)]
      (is (contains? (:nodes graph) '{:symbol test-ns/c-multi})
          "multimethod is defined")
      (is (contains? (:nodes graph) '{:symbol test-ns/c-multi :dispatch :hi})
          "specialization is defined")
      (is (contains? (:edges graph)
                     '{:caller {:symbol test-ns/c-multi} :call {:symbol clojure.core/identity}})
          "call from multimethod to dispatch exists")
      (is (contains? (:edges graph)
                     '{:caller {:symbol test-ns/c-multi :dispatch :hi} :call {:symbol clojure.core/println}})
          "call from specialization exists")))

  (testing "defmethod with external call"
    (let [graph (->> '(do
                        (defmulti d-multi identity)
                        (defmethod d-multi :hi [_]
                          (println "Hello world"))
                        (defn x [] (d-multi :hi)))
                     t.util/analyze+eval
                     parse/ast->graph
                     t.util/simplify-graph)]
      (is (every? (:nodes graph)
                  '#{{:symbol test-ns/d-multi}
                     {:symbol test-ns/d-multi :dispatch :hi}})
          "multimethod and specialization are defined")
      (is (every? (:edges graph)
                  '#{{:caller {:symbol test-ns/d-multi} :call {:symbol clojure.core/identity}}
                     {:caller {:symbol test-ns/d-multi :dispatch :hi} :call {:symbol clojure.core/println}}})
          "multimethod and specialization calls exist")
      (is (contains? (:nodes graph)
                     '{:symbol test-ns/x})
          "other fn is defined")
      (is (contains? (:edges graph)
                     '{:caller {:symbol test-ns/x} :call {:symbol test-ns/d-multi}})
          "other fn calls multimethod"))))

(comment
  ;; these are probably going to be split out into separate namespaces

(deftest ast->graph-protocol-test
  (testing "deftype with a protocol"
    (is (= '{:nodes #{{:symbol CProtocol}
                      {:symbol ->CType}
                      {:symbol c-method-1 :dispatch CType}
                      {:symbol c-method-2 :dispatch CType}}
             :edges #{{:caller {:symbol c-method-1 :dispatch CType} :call {:symbol clojure.core/println}}
                      {:caller {:symbol c-method-2 :dispatch CType} :call {:symbol clojure.core/map}}
                      {:caller {:symbol c-method-2 :dispatch CType} :call {:symbol clojure.core/inc}}}}
           (simple-graph '(do
                            (defprotocol CProtocol
                              (c-method-1 [this x])
                              (c-method-2 [this y]))
                            (deftype CType []
                              CProtocol
                              (c-method-1 [this x] (println x))
                              (c-method-2 [this y] (map inc y))))))))

  (testing "defrecord with a protocol"
    (let [{:keys [nodes edges]}
          (simple-graph '(do
                           (defprotocol DProtocol
                             (d-method-1 [this x])
                             (d-method-2 [this y]))
                           (defrecord DRecord []
                             DProtocol
                             (d-method-1 [this x] (println x))
                             (d-method-2 [this y] (map inc y)))))]
      ;; There are more auto-generated symbol and calls, but we only really
      ;; care about these
      (is (every?
           nodes
           '#{{:symbol DProtocol}
              {:symbol ->DRecord}
              {:symbol map->DRecord}
              {:symbol d-method-1 :dispatch DRecord}
              {:symbol d-method-2 :dispatch DRecord}}))
      (is (every?
           edges
           '#{{:caller {:symbol d-method-1 :dispatch DRecord} :call {:symbol clojure.core/println}}
              {:caller {:symbol d-method-2 :dispatch DRecord} :call {:symbol clojure.core/map}}
              {:caller {:symbol d-method-2 :dispatch DRecord} :call {:symbol clojure.core/inc}}}))))

  (testing "reify with a protocol"
    (let [graph (simple-graph '(do
                                 (defprotocol EProtocol
                                   (e-method-1 [this x])
                                   (e-method-2 [this y]))
                                 (reify EProtocol
                                   (e-method-1 [this x] (println x))
                                   (e-method-2 [this y] (map inc y)))))
          reify-type (some :dispatch (:nodes graph))]
      ;; This should be "reify--" plus a random number
      (is (re-find #"^reify-" (str reify-type))
          "reified type created")
      (is (= {:nodes #{{:symbol 'EProtocol}
                       {:symbol 'e-method-1 :dispatch reify-type}
                       {:symbol 'e-method-2 :dispatch reify-type}}
              :edges #{{:caller {:symbol 'e-method-1 :dispatch reify-type} :call {:symbol 'clojure.core/println}}
                       {:caller {:symbol 'e-method-2 :dispatch reify-type} :call {:symbol 'clojure.core/map}}
                       {:caller {:symbol 'e-method-2 :dispatch reify-type} :call {:symbol 'clojure.core/inc}}}}
             graph)))))

(deftest ast->graph-interface-test
  (testing "deftype with an interface"
    (is (= '{:nodes #{{:symbol ->IndexedType}
                      {:symbol clojure.lang.Indexed/nth :dispatch IndexedType}}
             :edges #{{:caller {:symbol clojure.lang.Indexed/nth :dispatch IndexedType} :call {:symbol clojure.core/first}}
                      {:caller {:symbol clojure.lang.Indexed/nth :dispatch IndexedType} :call {:symbol clojure.core/drop}}}}
           (simple-graph '(deftype IndexedType []
                            clojure.lang.Indexed
                            (nth [this n] (first (drop n this))))))))

  (testing "defrecord with an interface"
    (let [{:keys [nodes edges]}
          (simple-graph '(defrecord IndexedRecord []
                           clojure.lang.Indexed
                           (nth [this n] (first (drop n this)))))]
      ;; There are more auto-generated symbol and calls, but we only really
      ;; care about these
      (is (every?
           nodes
           '#{{:symbol ->IndexedRecord}
              {:symbol map->IndexedRecord}
              {:symbol clojure.lang.Indexed/nth :dispatch IndexedRecord}}))
      (is (every?
           edges
           '#{{:caller {:symbol clojure.lang.Indexed/nth :dispatch IndexedRecord} :call {:symbol clojure.core/first}}
              {:caller {:symbol clojure.lang.Indexed/nth :dispatch IndexedRecord} :call {:symbol clojure.core/drop}}}))))

  (testing "reify with an interface"
    (let [graph (simple-graph '(let [coll [1 2 3]]
                                 (reify clojure.lang.Indexed
                                   (nth [this n] (first (drop n coll))))))
          reify-type (some :dispatch (:nodes graph))]
      ;; This should be "reify--" plus a random number
      (is (re-find #"^reify-" (str reify-type))
          "reified type created")
      (is (= {:nodes #{{:symbol 'clojure.lang.Indexed/nth :dispatch reify-type}}
              :edges #{{:caller {:symbol 'clojure.lang.Indexed/nth :dispatch reify-type} :call {:symbol 'clojure.core/first}}
                       {:caller {:symbol 'clojure.lang.Indexed/nth :dispatch reify-type} :call {:symbol 'clojure.core/drop}}}}
             graph)))))

(deftest ast->graph-extend-test
  (testing "extend-type"
    (is (= '{:nodes #{{:symbol FProtocol}
                      {:symbol f-method-1 :dispatch java.lang.String}
                      {:symbol f-method-2 :dispatch java.lang.String}
                      {:symbol GProtocol}
                      {:symbol g-method-1 :dispatch java.lang.String}}
             :edges #{{:caller {:symbol f-method-1 :dispatch java.lang.String} :call {:symbol clojure.core/str}}
                      {:caller {:symbol f-method-2 :dispatch java.lang.String} :call {:symbol clojure.core/apply}}
                      {:caller {:symbol f-method-2 :dispatch java.lang.String} :call {:symbol clojure.core/str}}
                      {:caller {:symbol f-method-2 :dispatch java.lang.String} :call {:symbol clojure.core/repeat}}
                      {:caller {:symbol g-method-1 :dispatch java.lang.String} :call {:symbol clojure.core/apply}}
                      {:caller {:symbol g-method-1 :dispatch java.lang.String} :call {:symbol clojure.core/str}}
                      {:caller {:symbol g-method-1 :dispatch java.lang.String} :call {:symbol clojure.core/interpose}}}}
           (simple-graph '(do
                            (defprotocol FProtocol
                              (f-method-1 [this x])
                              (f-method-2 [this y]))
                            (defprotocol GProtocol
                              (g-method-1 [this z]))
                            (extend-type java.lang.String
                              FProtocol
                              (f-method-1 [this x] (str this x))
                              (f-method-2 [this y] (apply str (repeat y this)))
                              GProtocol
                              (g-method-1 [this z]
                                (apply str (interpose this z)))))))))

  (testing "extend-protocol"
    (is (= '{:nodes #{{:symbol HProtocol}
                      {:symbol h-method-1 :dispatch java.lang.String}
                      {:symbol h-method-1 :dispatch java.lang.Number}}
             :edges #{{:caller {:symbol h-method-1 :dispatch java.lang.String} :call {:symbol clojure.core/re-find}}
                      {:caller {:symbol h-method-1 :dispatch java.lang.Number} :call {:symbol h-method-1}}
                      {:caller {:symbol h-method-1 :dispatch java.lang.Number} :call {:symbol clojure.core/str}}}}
           (simple-graph '(do
                            (defprotocol HProtocol
                              (h-method-1 [this x]))
                            (extend-protocol HProtocol
                              java.lang.String
                              (h-method-1 [this x] (re-find x this))
                              java.lang.Number
                              (h-method-1 [this x]
                                (h-method-1 (str this) x)))))))))
)
