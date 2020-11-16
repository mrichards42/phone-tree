(ns phone-tree.parse.macro
  "defmacro and syntax quote parsing."
  (:require [clojure.tools.analyzer.ast :as ana.ast]
            [phone-tree.parse.ast :as ast]
            [phone-tree.parse.build :as build]))

(def ^:dynamic *in-macro* false)

;; defmacro expands to roughly
;;   (do
;;     `(defn ~name ~@body)
;;     (.setMacro ~name)
;;     (var ~name))
;; we only care about the defn part
(defmethod ast/parse 'clojure.core/defmacro
  [ast]
  (binding [*in-macro* true]
    (->> (ana.ast/nodes ast)
         (filter (comp #{:def} :op))
         (first)
         (ast/parse)
         (doall))))

;; handle syntax quote inside a macro as an edge
(defmethod ast/parse :quote
  [{:keys [expr] :as ast}]
  (if (and *in-macro* (= :symbol (:type expr)))
    (build/edge ast {:dest (:val expr)})
    (ast/parse-children expr)))

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

  (->> '(defmacro my-defn [& body]
          `(defn ~@body))
       clojure.tools.analyzer.jvm/analyze+eval
       ast/parse)

  (->> '(my-defn a [a b c]
          (+ 1 b c))
       clojure.tools.analyzer.jvm/analyze+eval
       ast/parse)

  )
