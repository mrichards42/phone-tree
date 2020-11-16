(ns phone-tree.parse.multimethod
  "Multimethod parsing"
  (:require [phone-tree.parse.ast :as ast]
            [phone-tree.parse.build :as build]))

;; NB: defmulti expands to a standard `def`, so we don't need to handle it
;; separately here.

;; (defmethod x :dispatch [] ...)
;; which turns into (. x addMethod :dispatch (fn [] ...))
(defmethod ast/parse 'clojure.lang.MultiFn/addMethod
  [{:keys [instance args] :as ast}]
  ;; - symbol is the multifn (an object that we're calling .addMethod on)
  ;; - dispatch is the first argument to .addMethod
  ;; - body is the second argument to .addMethod
  (let [[dispatch body] args
        multimethod-node {:symbol (:var instance) :dispatch (:form dispatch)}]
    (concat ;; the multimethod itself
            (build/node ast multimethod-node body)
            ;; edge from the main method
            (build/edge ast {:src (dissoc multimethod-node :dispatch)
                             :dest multimethod-node}))))

(comment
  (require 'clojure.tools.analyzer.jvm)

  (->> '(do
          (defmulti test-multi identity)
          (defmethod test-multi :x [x]
            (println x)))
       clojure.tools.analyzer.jvm/analyze+eval
       ast/parse)

  )
