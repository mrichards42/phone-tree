(ns phone-tree.parse.protocol
  "Protocol, record, type, and reify parsing"
  (:require [clojure.tools.analyzer.jvm.utils :refer [box]]
            [clojure.tools.analyzer.jvm :as ana.jvm]
            [clojure.tools.analyzer.ast :as ana.ast]
            [phone-tree.log :as log]
            [phone-tree.parse.ast :as ast]
            [phone-tree.parse.build :as build]
            [phone-tree.parse.class :refer [guess-class]]
            [phone-tree.util :as util]))

;;; Record methods
;; We don't include default record methods in the call graph

(def default-record-methods
  (delay
   (->> '(defrecord Test [])
        ana.jvm/analyze
        ana.ast/nodes
        (filter (comp #{:method} :op))
        (map (juxt :interface :name))
        set)))

(defn record-class? [cls]
  (.isAssignableFrom clojure.lang.IRecord cls))

(defn default-record-method?
  "Does this ast represent a default `defrecord` method?"
  [{:keys [this interface name] :as ast}]
  (and (record-class? (:tag this))
       (contains? @default-record-methods [interface name])))

;;; Protocol guessing

;; Much of this code copied or adapted from
;; https://github.com/clojure/clojure/blob/clojure-1.10.0/src/clj/clojure/core_deftype.clj#L535
(defn super-chain [^Class c]
  (when c
    (cons c (super-chain (.getSuperclass c)))))

(defn protocol-dispatch
  "Finds the class that implements the given protocol."
  [protocol cls]
  (if (isa? cls (:on-interface protocol))
    ;; If this is from a deftype or defrecord, the class _is_ the implementer
    cls
    ;; Otherwise search through superclasses
    (and cls (->> (super-chain (box cls))
                  (filter (partial get (:impls protocol)))
                  first))))

;;; AST Parsing

;; (defprotocol X (method-1 [this]) (method-2 [this]))
;; expands to (do ...) with a lot of stuff going on, but which returns the
;; symbol for the resulting protocol in the end

(defmethod ast/parse 'clojure.core/defprotocol
  [{:keys [result] :as ast}]
  (when-let [protocol (some-> (ast/ast-resolve ast result) var-get)]
    ;; pull all the methods off the protocol definition itself
    (flatten
     (for [k (keys (:method-builders protocol))]
       (let [protocol-fn (util/var->symbol k)
             interface-fn (symbol (.getName (:on-interface protocol))
                                  (munge (name protocol-fn)))]
         (concat ;; the interface method (no body, it's just a protocol)
                 (build/node ast interface-fn)

                 ;; the clojure function (no body, it's just a protocol)
                 (build/node ast protocol-fn)

                 ;; the `:method` op, which is used for method implementations
                 ;; in deftype and defrecord, only knows about the interface
                 ;; method, not the clojure protocol function. So it's simplest
                 ;; to have all the calls go through the java interface, e.g.
                 ;;
                 ;;   deftype ------\
                 ;;                  inferface method
                 ;;   protocol fn --/
                 ;;
                 (build/edge ast {:src protocol-fn, :dest interface-fn})))))))


;; method in a deftype, defrecord, or reify form

(defmethod ast/parse :method
  [{:keys [this interface name body] :as ast}]
  ;; - name is the method's name
  ;; - interface is method's interface (usually attached to a protocol)
  ;; - tag is the class (to dispatch on)
  ;; - body is the method's body
  (when-not (default-record-method? ast)
    (let [fn-name (munge (str name))
          interface-fn (symbol (.getName interface) fn-name)
          class-fn (symbol (.getName (:tag this)) fn-name)]
      (concat ;; the class method
              (build/node ast class-fn body)
              ;; a call from the interface-fn to the class method
              ;; this is equivalent to multimethod dispatch (on class)
              (build/edge ast {:src interface-fn, :dest class-fn})))))


;; (extend-type MyType
;;   ProtocolA
;;   (foo [])
;;   ProtocolB
;;   (bar []))
;; => (extend MyType
;;      ProtocolA {:foo (fn [])}
;;      ProtocolB {:bar (fn [])})
;;
;; (extend-protocol MyProtocol
;;   TypeA
;;   (foo [])
;;   TypeB
;;   (bar []))
;; => (do
;;      (extend TypeA
;;        MyProtocol {:foo (fn [])})
;;      (extend TypeB
;;        MyProtocol {:bar (fn [])}))
;;
;; (extend MyType MyProtocol (merge fn-map-a fm-map-b))
;; => we can't handle dynamically created maps like this

(defmethod ast/parse 'clojure.core/extend
  [{[the-cls & proto+maps] :args :as ast}]
  ;; extend takes multiple protocols and function maps:
  ;;
  ;;   (extend AType
  ;;     AProtocol
  ;;     {:some-fn (fn [] ...)
  ;;      ...}
  ;;     BProtocol
  ;;     {...})
  ;;
  (when-let [class-sym (some-> the-cls :val (.getName) symbol)]
    (flatten
     ;; 1. Outer loop -- each protocol and function map
     (for [[protocol-var method-map] (partition-all 2 proto+maps)
           :let [protocol (util/var->symbol (:var protocol-var))
                 protocol-ns (some-> protocol namespace)]]
       (cond
         (not protocol-ns)
         (log/warnf "Unable to parse extend without protocol: %s"
                    (:form ast))
         (not= :map (:op method-map))
         (log/warnf "Unable to parse extend with %s method-map: %s"
                    (:op method-map) (:form ast))
         :else
         ;; 2. Inner loop -- each k/v pair in the function map
         (for [[fn-kw body] (map vector (:keys method-map) (:vals method-map))
               ;; For each function:
               ;; - ns is the protocol's namespace
               ;; - name is the key in the function map
               ;; - the class (to dispatch on) is the type that is being extended
               ;; - body is the value in the function map
               :let [protocol-fn (some->> fn-kw :val name (symbol protocol-ns))]]
           (if-not protocol-fn
             (log/warnf "Unable to find protocol function in extend for %s"
                        (:form fn-kw))
             ;; Unlike :method, extend doesn't add _methods_ to a type, it just
             ;; adds _dispatches_ from the protocol. These extend functions
             ;; exist at (get-in MyProtocol [:impls some-class :fn-kw])
             (concat ;; the method, which dispatches on class
                     (build/node ast
                                 {:symbol protocol-fn, :dispatch class-sym}
                                 body)
                     ;; call the dispatch from the protocol-fn fn
                     (build/edge ast {:src {:symbol protocol-fn}
                                      :dest {:symbol protocol-fn
                                             :dispatch class-sym}})))))))))


;; (defprotocol MyProtocol
;;   (protocol-fn [this a b c]))
;;
;; (protocol-fn some-cls-or-type ... args) <-- this part

(defmethod ast/parse :protocol-invoke
  [{:keys [protocol-fn target args] :as ast}]
  (let [protocol (-> protocol-fn :meta :protocol var-get)
        protocol-fn (util/var->symbol (:var protocol-fn))
        interface (:on-interface protocol)
        target (guess-class target)
        to (if (isa? target interface)
             ;; class is an implementer -- use a class method
             {:symbol (symbol (.getName target) (munge (name protocol-fn)))}
             ;; class is not an implementer -- use the protocol function
             ;; itself; if we have a class, attempt to find a dispatch value
             (let [dispatch (protocol-dispatch protocol target)]
               (cond-> {:symbol protocol-fn}
                 dispatch (assoc :dispatch dispatch))))]
    (concat ;; edge to the protocol fn itself
            (build/edge ast {:dest to})
            ;; any args should be parsed as if they were children of the
            ;; enclosing scope
            (mapcat ast/parse (cons target args)))))


(comment
  (require 'clojure.tools.analyzer.jvm)

  (->> '(defprotocol X
          (method-1 [this a b c])
          (method-2 [this x]))
       clojure.tools.analyzer.jvm/analyze+eval
       ast/parse)

  )
