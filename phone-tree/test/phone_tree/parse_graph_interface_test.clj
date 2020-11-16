(ns phone-tree.parse-graph-interface-test
  "Tests for parse/ast->graph with interfaces, protocols, types, and records."
  (:require [clojure.test :refer [deftest testing is are]]
            [phone-tree.parse :as parse]
            [phone-tree.test-util :as t.util]))

(comment
  ;; Here's how things are compiled
  ;; Note: I'm using namespace.class/function syntax for interfaces and
  ;; classes, but these aren't actually defined as clojure vars

  (definterface A-B-C
    (abc_123 [x])) ;; NB: `-` not allowed in interface methods
  ;; This interface defines the following:
  ;; my_ns.A-B-C -- interface (NB: ns is munged, but name is not)
  ;; my_ns.A-B-C/abc_123 -- interface method (not a var)

  (defprotocol X-Y-Z
    (xyz-123 [this]))
  ;; This protocol defines the following:
  ;; my-ns/X-Y-Z -- protocol (var is actually a map)
  ;; my-ns/xyz-123 -- fn that dispatches on type (via the protocol)
  ;; my_ns.X_Y_Z -- interface (everything is munged)
  ;; my_ns.X_Y_Z/xyz_123 -- interface method (not a var)

  (deftype My-Type []
    X-Y-Z
    (xyz-123 [this])
    A-B-C
    (abc_123 [this x]))
  ;; This type defines the following:
  ;; my_ns.My-Type -- class (like interfaces, ns is munged, but name is not)
  ;; my_ns.My-Type/xyz_123 -- class methods (not vars)
  ;; my_ns.My-Type/abc_123

  ;; So the logical way to graph this is:

  ;; Protocol
  ;; my-ns/my-fn (protocol fn)
  ;;   -> my_ns.my_protocol/my_fn (interface method)
  ;;   -> my_ns.my-class/my_fn (class method for type/record)

  ;; Interface
  ;; my_ns.my-interface/my_fn (interface method)
  ;;   -> my_ns.my-class/my-fn (class method for type/record)
  )

(deftest definitions-test
  (testing "defprotocol"
    (let [graph (t.util/simple-graph
                 '(defprotocol Protocol-1
                    (method-1 [this x y z])))]
      (is (contains? (:nodes graph) {:symbol 'test-ns/method-1})
          "protocol function is defined in the current ns")
      (is (contains? (:nodes graph) {:symbol 'test_ns.Protocol_1/method_1})
          "protocol interface method is defined")
      (is (contains? (:edges graph) {:caller {:symbol 'test-ns/method-1}
                                     :call {:symbol 'test_ns.Protocol_1/method_1}})
          "call from protocol function to interface method exists")))

  (testing "definterface"
    (let [graph (t.util/simple-graph
                 '(definterface Interface-1
                    (imethod_1 [x y z])))]
      (is (empty? (:nodes graph))
          "defines no nodes")
      (is (empty? (:edges graph))
          "defines no edges"))))

(require '[phone-tree.graph :as g])

(comment
  ;; many different ways of calling reify things
  (def ast (t.util/simple-graph
   '(do
      (defprotocol HeyHey
        (hey [_]))

      (defn f [x y]
        (reify HeyHey
          (hey [_]
            (map #(str "hey " (.getName %)) [x y]))))
      (defn ff []
        (let [y (f Object Object)
              x (hey y)]
          x))
      (def a (f Object Object))
      (def b (hey a))
      )))
      (def ast (t.util/analyze+eval '(hey (identity a))))
      (->> '(do (def x (reify HeyHey (hey [_])))
              (hey x))
           t.util/simple-graph
       #_t.util/analyze+eval
       #_:target
       #_((juxt :op :class-name :tag))
       )
  )

(deftest types-test
  (let [graph (t.util/simple-graph
               '(do
                  (definterface Interface-2
                    (imethod_2 [x y z]))

                  (defprotocol Protocol-2
                    (method-2 [this x y z]))

                  (def type-2-imethod_2)
                  (def type-2-method-2)
                  (deftype Type-2 [a b c]
                    Interface-2
                    (imethod_2 [_ x y z]
                      type-2-imethod_2)
                    Protocol-2
                    (method-2 [_ x y z]
                      type-2-method-2))

                  (def record-2-imethod_2)
                  (def record-2-method-2)
                  (defrecord Record-2 [a b c]
                    Interface-2
                    (imethod_2 [_ x y z]
                      record-2-imethod_2)
                    Protocol-2
                    (method-2 [_ x y z]
                      record-2-method-2))

                  (def reify-2-imethod_2)
                  (def reify-2-method-2)
                  (def reified-2
                    (reify
                      Interface-2
                      (imethod_2 [this x y z]
                        reify-2-imethod_2)
                      Protocol-2
                      (method-2 [this x y z]
                        reify-2-method-2)))

                  (def extend-type-2-method-2)
                  (deftype ExtendType-2 [])
                  (extend-type ExtendType-2
                    Protocol-2
                    (method-2 [_ x y z]
                      extend-type-2-method-2))

                  (def string-method-2)
                  (extend-type java.lang.String
                    Protocol-2
                    (method-2 [_ x y z]
                      string-method-2))

                  (def number-method-2)
                  (def extend-protocol-2-method-2)
                  (deftype ExtendProtocol-2 [])
                  (extend-protocol Protocol-2
                    java.lang.Number
                    (method-2 [_ x y z]
                      number-method-2)
                    ExtendProtocol-2
                    (method-2 [_ x y z]
                      extend-protocol-2-method-2))))]

    (testing "Type"
      (testing "with Protocol"
        (is (contains? (:nodes graph)
                       {:symbol 'test_ns.Type-2/method_2})
            "type method is defined")
        (is (contains? (:edges graph)
                       {:caller {:symbol 'test_ns.Protocol_2/method_2}
                        :call {:symbol 'test_ns.Type-2/method_2}})
            "protocol method calls type method")
        (is (contains? (:edges graph)
                       {:caller {:symbol 'test_ns.Type-2/method_2}
                        :call {:symbol 'test-ns/type-2-method-2}})
            "type method external calls are defined"))
      (testing "with Interface"
        (is (contains? (:nodes graph)
                       {:symbol 'test_ns.Type-2/imethod_2})
            "type method is defined")
        (is (contains? (:edges graph)
                       {:caller {:symbol 'test_ns.Interface-2/imethod_2}
                        :call {:symbol 'test_ns.Type-2/imethod_2}})
            "interface method calls type method")
        (is (contains? (:edges graph)
                       {:caller {:symbol 'test_ns.Type-2/imethod_2}
                        :call {:symbol 'test-ns/type-2-imethod_2}})
            "type method external calls are defined")))

    (testing "Record"
      (is (not (some (:nodes graph)
                     '[{:symbol test_ns.Record-2/get}
                       {:symbol test_ns.Record-2/values}
                       {:symbol test_ns.Record-2/seq}
                       {:symbol test_ns.Record-2/assoc}
                       {:symbol test_ns.Record-2/cons}
                       {:symbol test_ns.Record-2/count}]))
          "default methods are not defined")
      (testing "with Protocol"
        (is (contains? (:nodes graph)
                       {:symbol 'test_ns.Record-2/method_2})
            "record method is defined")
        (is (contains? (:edges graph)
                       {:caller {:symbol 'test_ns.Protocol_2/method_2}
                        :call {:symbol 'test_ns.Record-2/method_2}})
            "protocol method calls record method")
        (is (contains? (:edges graph)
                       {:caller {:symbol 'test_ns.Record-2/method_2}
                        :call {:symbol 'test-ns/record-2-method-2}})
            "record method external calls are defined"))
      (testing "with Interface"
        (is (contains? (:nodes graph)
                       {:symbol 'test_ns.Record-2/imethod_2})
            "record method is defined")
        (is (contains? (:edges graph)
                       {:caller {:symbol 'test_ns.Interface-2/imethod_2}
                        :call {:symbol 'test_ns.Record-2/imethod_2}})
            "interface method calls record method")
        (is (contains? (:edges graph)
                       {:caller {:symbol 'test_ns.Record-2/imethod_2}
                        :call {:symbol 'test-ns/record-2-imethod_2}})
            "record method external calls are defined")))

    (testing "reify"
      (let [reify-ns (some->> (:nodes graph)
                              (map (comp str :symbol))
                              (some (partial re-find #".*\$reify__.*/"))
                              symbol
                              namespace)]
        (is (some? reify-ns) "reify class exists")
      (testing "with Protocol"
        (is (contains? (:nodes graph)
                       {:symbol (symbol reify-ns "method_2")})
            "reify method is defined")
        (is (contains? (:edges graph)
                       {:caller {:symbol 'test_ns.Protocol_2/method_2}
                        :call {:symbol (symbol reify-ns "method_2")}})
            "protocol method calls reify method")
        (is (contains? (:edges graph)
                       {:caller {:symbol (symbol reify-ns "method_2")}
                        :call {:symbol 'test-ns/reify-2-method-2}})
            "reify method external calls are defined"))
      (testing "with Interface"
        (is (contains? (:nodes graph)
                       {:symbol (symbol reify-ns "imethod_2")})
            "reify method is defined")
        (is (contains? (:edges graph)
                       {:caller {:symbol 'test_ns.Interface-2/imethod_2}
                        :call {:symbol (symbol reify-ns "imethod_2")}})
            "interface method calls reify method")
        (is (contains? (:edges graph)
                       {:caller {:symbol (symbol reify-ns "imethod_2")}
                        :call {:symbol 'test-ns/reify-2-imethod_2}})
            "reify method external calls are defined"))))

    (testing "extend-type"
      (testing "with Type"
        (is (contains? (:nodes graph)
                       {:symbol 'test-ns/method-2
                        :dispatch 'test_ns.ExtendType-2})
            "protocol function with dispatch exists")
        (is (contains? (:edges graph)
                       {:caller {:symbol 'test-ns/method-2}
                        :call {:symbol 'test-ns/method-2
                               :dispatch 'test_ns.ExtendType-2}})
            "protocol function calls dispatch function")
        (is (contains? (:edges graph)
                       {:caller {:symbol 'test-ns/method-2
                                 :dispatch 'test_ns.ExtendType-2}
                        :call {:symbol 'test-ns/extend-type-2-method-2}})
            "dispatch function external calls are defined"))

      (testing "with class"
        (is (contains? (:nodes graph)
                       {:symbol 'test-ns/method-2
                        :dispatch 'java.lang.String})
            "protocol function with dispatch exists")
        (is (contains? (:edges graph)
                       {:caller {:symbol 'test-ns/method-2}
                        :call {:symbol 'test-ns/method-2
                               :dispatch 'java.lang.String}})
            "protocol function calls dispatch function")
        (is (contains? (:edges graph)
                       {:caller {:symbol 'test-ns/method-2
                                 :dispatch 'java.lang.String}
                        :call {:symbol 'test-ns/string-method-2}})
            "dispatch function external calls are defined")))

    (testing "extend-protocol"
      (testing "with Type"
        (is (contains? (:nodes graph)
                       {:symbol 'test-ns/method-2
                        :dispatch 'test_ns.ExtendProtocol-2})
            "protocol function with dispatch exists")
        (is (contains? (:edges graph)
                       {:caller {:symbol 'test-ns/method-2}
                        :call {:symbol 'test-ns/method-2
                               :dispatch 'test_ns.ExtendProtocol-2}})
            "protocol function calls dispatch function")
        (is (contains? (:edges graph)
                       {:caller {:symbol 'test-ns/method-2
                                 :dispatch 'test_ns.ExtendProtocol-2}
                        :call {:symbol 'test-ns/extend-protocol-2-method-2}})
            "dispatch function external calls are defined"))

      (testing "with class"
        (is (contains? (:nodes graph)
                       {:symbol 'test-ns/method-2
                        :dispatch 'java.lang.Number})
            "protocol function with dispatch exists")
        (is (contains? (:edges graph)
                       {:caller {:symbol 'test-ns/method-2}
                        :call {:symbol 'test-ns/method-2
                               :dispatch 'java.lang.Number}})
            "protocol function calls dispatch function")
        (is (contains? (:edges graph)
                       {:caller {:symbol 'test-ns/method-2
                                 :dispatch 'java.lang.Number}
                        :call {:symbol 'test-ns/number-method-2}})
            "dispatch function external calls are defined")))
  ))

;; TODO: deal w/ the above + calls
;; TODO: might want some separate reify tests to make sure we aren't clobbering
;; multiple reifies from the same ns or function
;; TODO: below here is probably junk

(deftest ast->graph-protocol+type-test
  (testing "Simple protocol and type"
    (let [graph (-> '(do
                       (defprotocol Protocol-2
                         (method-2 [this x y z]))

                       (deftype Type-2 [a b c]
                         Protocol-2
                         (method-2 [_ x y z]
                           (println (str "Hello " x) y z))))
                    t.util/analyze+eval
                    parse/ast->graph
                    t.util/simplify-graph)]
      (is (every? (:nodes graph)
                  '#{{:symbol test_ns.Protocol_2/method_2}
                     {:symbol test-ns/method-2}})
          "protocol function and interface method are defined")
      (is (contains? (:edges graph) '{:caller {:symbol test-ns/method-2}
                                      :call {:symbol test_ns.Protocol_2/method_2}})
          "call from protocol function to interface method exists")
      (is (every? (:nodes graph)
                  '#{{:symbol test-ns/->Type-2}
                     {:symbol test_ns.Type-2/method_2}})
          "type constructor and method are defined")
      (is (contains? (:edges graph)
                     '{:caller {:symbol test_ns.Protocol_2/method_2}
                       :call {:symbol test_ns.Type-2/method_2}})
          "call from interface method to type method (dispatch) exists")
      (is (every? (:edges graph)
                  '#{{:caller {:symbol test_ns.Type-2/method_2}
                      :call {:symbol clojure.core/println}}
                     {:caller {:symbol test_ns.Type-2/method_2}
                      :call {:symbol clojure.core/str}}})
          "calls from type method to external functions exist")))

  (testing "Protocol call without explicit type information"
    (let [graph (-> '(let [instance (->Type-2 1 2 3)]
                       (defn wrapper-2a [x y z] (method-2 instance x y z)))
                    t.util/analyze+eval
                    parse/ast->graph
                    t.util/simplify-graph)]
      (is (contains? (:edges graph)
                     '{:caller {:symbol test-ns/wrapper-2a}
                       :call {:symbol test-ns/method-2}})
          "call is on the protocol function for local"))
    ;; TODO: might want to break this out into its own test since there are a
    ;; _lot_ of potential variations:
    (comment
      ;; 1 def
        ;; 1a def w/ protocol-fn
          ;; 1aa: def, protocol-fn, type constructor fn
          (def instance (->Type))
          (method instance args)
          ;; 1ab: def, protocol-fn, type constructor fn
          (def instance (Type.))
          (method instance args)
        ;; 1b def w/ method call
          ;; 1ba: def, method, type constructor fn
          (def instance (->Type))
          (.method instance args)
          ;; 1bb: def, method, type constructor fn
          (def instance (Type.))
          (.method instance args)
      ;; 2 all of the above, but with `let` instead of def
      ;; then rinse, repeat w/ records, interfaces, and reify
      )
    (let [graph (-> '(do 
                       (def instance-2aa (->Type-2 1 2 3))
                       (defn wrapper-2aa [x y z] (method-2 instance-2aa x y z)))
                    t.util/analyze+eval
                    parse/ast->graph
                    t.util/simplify-graph)]
      (is (contains? (:edges graph)
                     ;; For some reason making this a var allows us to figure
                     ;; out the var's type, but we can't do the same for let
                     ;; bindings
                     '{:caller {:symbol test-ns/wrapper-2aa}
                       :call {:symbol test_ns.Type-2/method_2}})
          "call is on the type method for var")))

  (testing "Protocol call with a known class"
    (let [graph (-> '(let [instance (Type-2. 1 2 3)]
                       (defn wrapper-2b [x y z] (method-2 instance x y z)))
                    t.util/analyze+eval
                    parse/ast->graph
                    t.util/simplify-graph)]
      (is (contains? (:edges graph)
                     '{:caller {:symbol test-ns/wrapper-2b}
                       :call {:symbol test_ns.Type-2/method_2}})
          "call is on the type's method")))

  (testing "Instance call with no type information"
    (let [graph (-> '(let [instance (->Type-2 1 2 3)]
                       (defn wrapper-2c [x y z] (.method-2 instance x y z)))
                    t.util/analyze+eval
                    parse/ast->graph
                    t.util/simplify-graph)]
      (is (contains? (:edges graph)
                     '{:caller {:symbol test-ns/wrapper-2c}
                       ;; calls on java.lang.Object get an unmunged method
                       :call {:symbol java.lang.Object/method-2}})
          "call is on a java.lang.Object method")))

  (testing "Instance call with a known type"
    (let [graph (-> '(let [instance (Type-2. 1 2 3)]
                       (defn wrapper-2d [x y z] (.method-2 instance x y z)))
                    t.util/analyze+eval
                    parse/ast->graph
                    t.util/simplify-graph)]
      (is (contains? (:edges graph)
                     '{:caller {:symbol test-ns/wrapper-2d}
                       :call {:symbol test_ns.Type-2/method_2}})
          "call is on the interface method for the type"))))

(deftest ast->graph-interface-test
  (let [graph (-> '(definterface Interface-1
                     (imethod_1 [x y z]))
                  t.util/analyze+eval
                  parse/ast->graph
                  t.util/simplify-graph)]
    (is (empty? (:nodes graph))
        "interface defines no nodes")
    (is (empty? (:edges graph))
        "interface defines no edges")))

(deftest ast->graph-interface+type-test
  (testing "Simple interface and type"
    (let [graph (-> '(do
                       (definterface Interface-2
                         (imethod_2 [x y z]))

                       (deftype IType-2 [a b c]
                         Interface-2
                         (imethod_2 [_ x y z]
                           (println (str "Hello " x) y z))))
                    t.util/analyze+eval
                    parse/ast->graph
                    t.util/simplify-graph)]
      (is (not (some #{'test-ns/imethod_2}
                     (concat (map :symbol (:nodes graph))
                             (map (comp :symbol :caller) (:edges graph))
                             (map (comp :symbol :call) (:edges graph)))))
          "interface method is not defined as a namespace function")
      (is (every? (:nodes graph)
                  '#{{:symbol test-ns/->IType-2}
                     {:symbol test_ns.IType-2/imethod_2}})
          "type constructor and method are defined")
      (is (contains? (:edges graph)
                     '{:caller {:symbol test_ns.Interface-2/imethod_2}
                       :call {:symbol test_ns.IType-2/imethod_2}})
          "call from interface method to type method (dispatch) exists")
      (is (every? (:edges graph)
                  '#{{:caller {:symbol test_ns.IType-2/imethod_2}
                      :call {:symbol clojure.core/println}}
                     {:caller {:symbol test_ns.IType-2/imethod_2}
                      :call {:symbol clojure.core/str}}})
          "calls from type method to external functions exist")))

  (testing "Instance call with no type information"
    (let [graph (-> '(let [instance (->IType-2 1 2 3)]
                       (defn iwrapper-2a [x y z] (.imethod_2 instance x y z)))
                    t.util/analyze+eval
                    parse/ast->graph
                    t.util/simplify-graph)]
      (is (contains? (:edges graph)
                     '{:caller {:symbol test-ns/iwrapper-2a}
                       :call {:symbol java.lang.Object/imethod_2}})
          "call is on a java.lang.Object method")))

  (testing "Instance call with a known type"
    (let [graph (-> '(let [instance (IType-2. 1 2 3)]
                       (defn iwrapper-2b [x y z] (.imethod_2 instance x y z)))
                    t.util/analyze+eval
                    parse/ast->graph
                    t.util/simplify-graph)]
      (is (contains? (:edges graph)
                     '{:caller {:symbol test-ns/iwrapper-2b}
                       :call {:symbol test_ns.IType-2/imethod_2}})
          "call is on the interface method for the type"))))

(deftest ast->graph-protocol+record-test
  (let [graph (-> '(do
                     (defprotocol Protocol-3
                       (method-3 [this x y z]))

                     (defrecord Record-3 [a b c]
                       Protocol-3
                       (method-3 [_ x y z]
                         (println (str "Hello " x) y z))))
                  t.util/analyze+eval
                  parse/ast->graph
                  t.util/simplify-graph)]
    (is (every? (:nodes graph)
                '#{{:symbol test-ns/->Record-3}
                   {:symbol test-ns/map->Record-3}
                   {:symbol test_ns.Record-3/method_3}})
        "record constructors and method are defined")
    (is (not (some (:nodes graph)
                   '[{:symbol test_ns.Record-3/get}
                     {:symbol test_ns.Record-3/values}
                     {:symbol test_ns.Record-3/seq}
                     {:symbol test_ns.Record-3/assoc}
                     {:symbol test_ns.Record-3/cons}
                     {:symbol test_ns.Record-3/count}]))
        "default record methods are not defined")
    (is (contains? (:edges graph)
                   '{:caller {:symbol test_ns.Protocol_3/method_3}
                     :call {:symbol test_ns.Record-3/method_3}})
        "call from interface method to record method (dispatch) exists")
    (is (every? (:edges graph)
                '#{{:caller {:symbol test_ns.Record-3/method_3}
                    :call {:symbol clojure.core/println}}
                   {:caller {:symbol test_ns.Record-3/method_3}
                    :call {:symbol clojure.core/str}}})
        "calls from record method to external functions exist")))

(deftest ast->graph-protocol+reify-test
  (testing "Simple protocol and reify"
    (let [graph (-> '(do
                       (defprotocol Protocol-4
                         (method-4 [this x y z]))

                       (def rinstance-4
                         (reify Protocol-4
                           (method-4 [_ x y z]
                             (println (str "Hello " x) y z)))))
                    t.util/analyze+eval
                    parse/ast->graph
                    t.util/simplify-graph)
          reified-ns (some->> (:nodes graph)
                              (map :symbol)
                              (filter #(re-find #"\$reify" (namespace %)))
                              first
                              namespace)
          reified-method (symbol reified-ns "method_4")]
      (is (contains? (:nodes graph) {:symbol reified-method})
          "reified method is defined")
      (is (contains? (:edges graph)
                     {:caller {:symbol 'test_ns.Protocol_4/method_4}
                      :call {:symbol reified-method}})
          "call from interface method to reified method (dispatch) exists")
      (is (every? (:edges graph)
                  #{{:caller {:symbol reified-method}
                     :call {:symbol 'clojure.core/println}}
                    {:caller {:symbol reified-method}
                     :call {:symbol 'clojure.core/str}}})
          "calls from reified method to external functions exist")
      ;; TODO: should this be a reference between the var and the _method_, or
      ;; between the var and the reified class?
      ;; or should this node even exist in the first place?
      (is (contains? (:edges graph)
                     {:caller {:symbol 'test-ns/rinstance-4}
                      :call {:symbol reified-method}})
          "reference between reified var and reified method exists")))
  (testing "Instance call"
    (let [graph (-> '(defn rwrapper-4a [x y z] (.method_4 rinstance-4 x y z))
                    t.util/analyze+eval
                    parse/ast->graph
                    t.util/simplify-graph)]
      (is (contains? (:edges graph)
                     '{:caller {:symbol test-ns/iwrapper-2b}
                       :call {:symbol test_ns.IType-2/imethod_2}})
          "call is on the interface method for the type")))

  (testing "Protocol call"
    (let [graph (-> '(let [i (reify Protocol-4 (method-4 [_ x y z]))]
                       (defn rwrapper-4b [x y z] (.method-4 i x y z)))
                    t.util/analyze+eval
                    parse/ast->graph
                    t.util/simplify-graph)]
      (is (contains? (:edges graph)
                     '{:caller {:symbol test-ns/iwrapper-2b}
                       :call {:symbol test_ns.IType-2/imethod_2}})
          "call is on the interface method for the type")))
  )

;; TODO: extend

(-> '(.toString 1) t.util/analyze+eval parse/ast->graph)

