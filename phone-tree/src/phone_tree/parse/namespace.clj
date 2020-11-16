(ns phone-tree.parse.namespace
  "Clojure namespace parsing."
  (:require [phone-tree.parse.ast :as ast]
            [phone-tree.parse.build :as build]))

(defmethod ast/parse 'clojure.core/in-ns
  [{:keys [result args] :as ast}]
  (build/node ast {:symbol (ns-name result) :type :namespace} args))
