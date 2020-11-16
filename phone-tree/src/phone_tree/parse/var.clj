(ns phone-tree.parse.var
  "Clojure var parsing (def forms and function calls)."
  (:require [phone-tree.parse.ast :as ast]
            [phone-tree.parse.build :as build]
            [phone-tree.parse.class :refer [guess-class]]))

;; (def x 10)
;; (def x (fn [] ...)) aka (defn x [] ...)
;; (defmulti x (fn [] ...))
(defmethod ast/parse :def
  [{:keys [var init] :as ast}]
  ;; - symbol is the node's :var
  ;; - body is the node's :init
  (build/node ast var init))

;; clojure function invocations and function arguments
(defmethod ast/parse :var
  [{:keys [var] :as ast}]
  (build/edge ast {:dest var}))

;; (var x)
;; #'x
(defmethod ast/parse :the-var
  [ast]
  (build/edge ast {:dest (:var ast)}))



(comment
  (require 'clojure.tools.analyzer.jvm)

  (->> '(def x '(1 2 3))
       clojure.tools.analyzer.jvm/analyze
       ast/parse)

  (->> '(def x (map identity '(1 2 3)))
       clojure.tools.analyzer.jvm/analyze+eval
       ast/parse)

  (->> '(map identity '(1 2 3))
       clojure.tools.analyzer.jvm/analyze+eval
       ast/parse)

  (->> '(def x #'map)
       clojure.tools.analyzer.jvm/analyze
       ast/parse)

  (->> '(defn x [a b c] (+ a b c))
       clojure.tools.analyzer.jvm/analyze+eval
       ast/parse)

  )
