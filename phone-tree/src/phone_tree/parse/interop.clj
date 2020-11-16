(ns phone-tree.parse.interop
  "Interop call parsing."
  (:require [phone-tree.parse.ast :as ast]
            [phone-tree.parse.build :as build]
            [phone-tree.parse.class :refer [guess-class]]))

;; e.g (.toLowerCase "some string")

(defmethod ast/parse :instance-call
  [{:keys [instance method args] :as ast}]
  (let [cls (or (guess-class instance)
                (guess-class ast)
                java.lang.Object)
        ;; method is already munged if this is from a clojure protocol
        method-sym (symbol (.getName cls) (str method))]
    (concat ;; edge to the protocol fn itself
            (build/edge ast {:dest method-sym})
            ;; args are parsed as children of the enclosing scope
            (ast/parse-children instance args))))


;; "Node for a no-arg instance-call or for an instance-field that couldn't be
;; resolved at compile time"
;; http://clojure.github.io/tools.analyzer.jvm/spec/quickref.html#host-interop

(defmethod ast/parse :host-interop
  [{:keys [m-or-f target] :as ast}]
  (let [;; By definition, this op-code means we have no type information, but
        ;; give it a guess anyways
        cls (or (guess-class target)
                (guess-class ast)
                java.lang.Object)
        method-sym (symbol (.getName cls) (str m-or-f))]
    ;; By definition, this is a 0-arg function or a field, so there are no
    ;; other child args to parse
    (build/edge ast {:dest method-sym})))


;; (java.util.UUID/fromString "")

(defmethod ast/parse :static-call
  [{:keys [class method args] :as ast}]
  ;; java static method invocations
  (let [cls (or (guess-class ast)
                class
                java.lang.Object)
        method-sym (symbol (.getName cls) (str method))]
    (concat ;; edge to the method
            (build/edge ast {:dest method-sym})
            ;; args are parsed as children of the enclosing scope
            (ast/parse-children args))))

;; (java.lang.String. "Hello world")

(defmethod ast/parse :new
  [{:keys [class args] :as ast}]
  (let [cls (or (guess-class class)
                (guess-class ast))]
    (concat ;; edge to the constructor
            (build/edge ast {:dest (symbol (.getName cls) ".")})
            ;; args are parsed as children of the enclosing scope
            (ast/parse-children args))))


(comment
  (require 'clojure.tools.analyzer.jvm)

  (defprotocol X
    (method-1 [this a b c])
    (method-2 [this x]))

  (deftype T []
    X
    (method-1 [this a b c])
    (method-2 [this x]))

  (->> '(method-1 (->T) 1 2 3)
       clojure.tools.analyzer.jvm/analyze+eval
       ast/parse)

  )
