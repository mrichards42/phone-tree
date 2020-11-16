(ns phone-tree.parse.ast
  "AST parsing functions"
  (:require [clojure.tools.analyzer.ast :as ana.ast]
            [phone-tree.util :as util]))

(defn location
  "Selects the location metadata from an ast node."
  [ast]
  (select-keys (:env ast) [:file :ns :line :end-line :column :end-column]))

(defn form
  "Returns the original form for this ast."
  [ast]
  (or (first (:raw-forms ast))
      (:form ast)))

(defn ast-resolve
  "Like ns-resolve, but using the :ns from the ast's :env"
  [ast sym]
  (when-let [env-ns (some-> ast :env :ns find-ns)]
    (when (symbol? sym)
      (ns-resolve env-ns sym))))

(defn first-form-var
  "Returns the first item of the original form, if it is a var used in a
  function or macro call position."
  [ast]
  ;; If the original form was a macro, :raw-forms is a sequence of
  ;; macroexpansions, so the first one is the original form. If the original
  ;; form was not a macro, :form is the original form.
  (let [raw-form (form ast)]
    (when (list? raw-form)
      (ast-resolve ast (first raw-form)))))

(defn dispatches
  "Returns a seq of potential dispatch vals for an `ast` node, in decreasing
  precedence:

  - the java method for an `:instance-call` or `:static-call` node
    (as a symbol: 'java.class/method-name)
  - the clojure function for an `:invoke` node
    (as a symbol: 'clojure.ns/fn-name)
  - the clojure function or macro in the original form, for any node
    (as a symbol: 'clojure.ns/fn-name)
  - the ast `:op`, for any node (a keyword)"
  [{:keys [op] :as ast}]
  (->> [;; java.class/method-name
        (when (#{:instance-call :static-call} op)
          (when-let [cls (:class ast)]
            (symbol (.getName cls) (str (:method ast)))))
        ;; clojure.ns/fn-name
        (when (= :invoke op)
          (some-> ast :fn :var util/var->symbol))
        ;; first var in the original form (a macro or function)
        (some-> (first-form-var ast) util/var->symbol)
        ;; raw op
        op]
       (filter identity)
       (distinct)))

(declare parse)

(defn parse-dispatch [ast]
  ;; In order to return only the most specific of the possibilities returned by
  ;; `dispatches`, we need to examine the actual defined methods
  (->> (dispatches ast)
       (filter (partial contains? (methods parse)))
       (first)))

(defmulti parse
  "Parses an `ast` into an intermediate graph representation (a combined seq of
  nodes and edges).

  Dispatches on the most specific of:

  - java method (as a symbol)
  - clojure function or macro (fully qualified symbol)
  - ast node :op (keyword)"
  parse-dispatch)

(defn parse-children
  "Parses any combination of ast nodes or sequences of ast nodes."
  [& children]
  (mapcat parse (flatten children)))

(defmethod parse :default
  [ast]
  (parse-children (ana.ast/children ast)))
