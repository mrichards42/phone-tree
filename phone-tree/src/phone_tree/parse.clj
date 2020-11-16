(ns phone-tree.parse
  "Functions for generating call graphs from namespaces.

  A call graph is represented as an `ubergraph` digraph.

  Each defined var, class, protocol, etc. is a node in the graph. Each
  reference from one var to another (e.g. a function call, but any use is
  counted) is an edge in the graph.

  Node ids are formed by concatenating the symbol with the dispatch value (if
  any). Nodes have the following attributes:

    :symbol    -- the var that was defined (as a fully-qualified symbol)
    :dispatch  -- the dispatch value (for multimethods and types/records)
    :count     -- number of times this var was defined
    :locations -- a set of location metadata (:ns, :file, :line, etc.)
    :forms     -- a set of the original forms that defined this var
    :external? -- true for nodes that were not defined in an analyzed namespace

  Edges have the following attributes:

    :count     -- number of times the edge's src called the edge's dest
    :locations -- a set of location metadata (:ns, :file, :line, etc.)

  Note: vars can be defined multiple times, and one function may call another
  multiple times. For the sake of simplicity, these cases are still represented
  as single nodes and edges in the graph, and the `:count` and `:locations`
  attributes can be used to identify each def or call."
  (:require [clojure.tools.analyzer.jvm :as ana.jvm]
            [phone-tree.log :as log]
            [phone-tree.parse.ast :as ast]
            [phone-tree.parse.build :as build]
            ;; parsing implementations
            [phone-tree.parse.interop]
            [phone-tree.parse.multimethod]
            #_[phone-tree.parse.namespace]
            [phone-tree.parse.protocol]
            [phone-tree.parse.var]))

(defn parse-ns [sym]
  (log/debugf "Parsing namespace: %s" sym)
  (mapcat ast/parse (ana.jvm/analyze-ns sym)))

(defn ns->graph
  "Returns a call graph for a namespace symbol."
  [sym]
  (build/ubergraph (parse-ns sym)))

(defn nss->graph
  "Returns a call graph for a number of namespaces."
  [nss]
  (build/ubergraph (mapcat parse-ns nss)))




;; TODO: do we care about this?
;; if so, we probably need to change the implementation to work with the new
;; graph code

(defn ast-meta->graph
  "Returns a map of edges and nodes defined in a var's metadata.

  Vars may attach the following metadata:

  {:phone-tree/nodes ['symbol ...]
   :phone-tree/edges [{:src 'symbol :dest 'symbol} ...]}

  Unqualified symbols will use the current namespace.

  Nodes may also be a map with :symbol and :dispatch keys for multiple-dispatch
  functions and calls."
  [ast]
  (letfn [(qualify-symbol [x] (if (namespace x)
                                x
                                (symbol (-> ast :meta :ns ns-name str)
                                        (name x))))
          (->node [x]
            (let [n (if (map? x) x {:symbol x})]
              (update n :symbol qualify-symbol)))]
    (merge
      (when-let [edges (-> ast :meta :phone-tree/edges)]
        {:edges (for [{:keys [caller call]} edges]
                  {:caller (->node caller)
                   :call (->node call)
                   :location (ast/location ast)})})
      (when-let [nodes (-> ast :meta :phone-tree/nodes)]
        {:nodes (for [n (map ->node nodes)]
                  (assoc n :location (ast/location ast)))}))))








;; TODO: below here is probably junk now

(comment

(defn parse-one
  "Parses a single ast node into an intermediate call-graph form.
  Returns a seq of maps with the following keys:

    :def      -- defines a symbol and sets the current symbol
    :method   -- defines a method and sets the current symbol
    :call     -- adds a call (or reference) from the current symbol
    :edge     -- adds a synthetic edge (with :caller, :call, and :location keys)
    :children -- child nodes for a :def or :call

  Note: :def adds an edge, whereas :method does not."
  [ast]
  (util/make-seq (parse.ast/parse ast)))

(def ast-resolve parse.ast/ast-resolve)

(defn first-raw-form
  [ast]
  (let [raw-form (-> ast :raw-forms first)]
    (when (list? raw-form)
      (first raw-form))))

(defn op-type
  "Returns the operation type from an ast node.

  One of:

    :def       -- def, defn, defmulti
    :method    -- deftype, defrecord, reify
    :defmethod -- defmethod
    :extend    -- extend-type, extend-protocol
    :var       -- a var
  ;; TODO: finish this docstring"
  [{:keys [op] :as ast}]
  (cond
    ;; defmethod
    ;; => (. some-multifn addMethod dispatch-value (fn []))
    (and (= :instance-call op)
         (= clojure.lang.MultiFn (:class ast))
         (= 'addMethod (:method ast)))
    :defmethod

    ;; extend-type and extend-protocol
    ;; => (extend some-class some-protocol methods*)
    (and (= :invoke op)
         (= #'extend (get-in ast [:fn :var])))
    :extend

    ;; defprotocol
    (= #'defprotocol (ast-resolve ast (first-raw-form ast)))
    :defprotocol

    ;; :def, :method, :var, and function invocations
    ;; (:protocol-invoke :static-call :instance-call :new)
    :else op))

(defn ast->location
  "Selects the location data from an ast node."
  [ast]
  (select-keys (:env ast) [:file :line :ns]))

(defmulti parse-one* op-type)


(defmethod parse-one* :default
  [ast]
  {:children (ana.ast/children ast)})

;; -- defs

(defmethod parse-one* :def
  [{:keys [var init] :as ast}]
  ;; (def x 10) or (def x (fn [] ...)) or (defmulti x (fn [] ...))
  ;; - symbol is the node's :var
  ;; - body is the node's :init
  {:def {:symbol (util/var->symbol var)
         :location (ast->location ast)}
   :children [init]})

(defmethod parse-one* :defmethod
  [{:keys [var instance args] :as ast}]
  ;; defmethod => (. some-multifn addMethod dispatch-value (fn []))
  ;; - symbol is the multifn (an object that we're calling .addMethod on)
  ;; - dispatch is the first argument to .addMethod
  ;; - body is the second argument to .addMethod
  (let [[dispatch body] args]
    {:def {:symbol (util/var->symbol var)
           :dispatch (:form dispatch)
           :location (ast->location ast)}
     :children [body]}))

(defmethod parse-one* :defprotocol
  [{:keys [result] :as ast}]
  (when-let [protocol (some-> (ast-resolve ast result) var-get)]
    ;; define all the methods
    (flatten
     (for [v (-> protocol :method-builders keys)]
       (let [protocol-fn (util/var->symbol v)
             interface-fn (symbol (.getName (:on-interface protocol))
                                  (munge (name protocol-fn)))]
         [;; the interface method
          {:method {:symbol interface-fn
                    :location (ast->location ast)}}
          ;; the clojure function
          {:def {:symbol protocol-fn
                 :location (ast->location ast)}}
          ;; we only have access to the interface (not the protocol) in the
          ;; :method op, so it's simplest to have the clojure function call
          ;; the interface method in the graph
          {:edge {:caller {:symbol protocol-fn}
                  :call {:symbol interface-fn}
                  :location (ast->location ast)}}])))))

(defmethod parse-one* :reify
  [ast]
  ;; See the note at the top of parse-class for an explanation of how we handle
  ;; reify forms
  (parse-class/record-reify! (:class-name ast) (meta (:form ast)))
  {:children (ana.ast/children ast)})

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
  [{:keys [this interface name] :as ast}]
  (and (record-class? (:tag this))
       (contains? @default-record-methods [interface name])))

(defmethod parse-one* :method
  [{:keys [this interface name body] :as ast}]
  ;; a type/record method definition
  ;; - the function/method name is the node's :name
  ;; - the interface (usually a protocol) is the node's :interface
  ;; - the class (to dispatch on) is the node's :tag
  ;; - body is the node's :body
  (when-not (default-record-method? ast)
    (let [fn-name (munge (str name))
          interface-fn (symbol (.getName interface) fn-name)
          class-fn (symbol (.getName (:tag this)) fn-name)]
      (list
       ;; the class method
       {:method {:symbol class-fn
                 :location (ast->location ast)}
        :children [body]}
       ;; a call from the interface-fn to the class method
       ;; this is equivalent to multimethod dispatch (on class)
       {:edge {:caller {:symbol interface-fn}
               :call {:symbol class-fn}
               :location (ast->location ast)}}))))

;; NB: this only parses `extend-protocol` and `extend-type` macros, and
;; `extend` calls with static maps, but doesn't handle `extend` calls with
;; dynamically constructed maps
(defmethod parse-one* :extend
  [{[dispatch-cls & proto+maps] :args :as ast}]
  ;; an extend call (usually extend-type or extend-protocol)
  ;; extend is complicated -- it takes multiple protocols and function maps:
  ;;
  ;;   (extend AType
  ;;     AProtocol
  ;;     {:some-fn (fn [] ...)
  ;;      ...}
  ;;     BProtocol
  ;;     {...})
  ;;
  ;; we need nested loops for this -- an outer loop for each protocol +
  ;; function map; and an inner loop for each function in the map
  (when-let [class-sym (some-> dispatch-cls :val (.getName) symbol)]
    (flatten
     ;; 1. Outer loop -- each protocol and function map
     (for [[protocol-var method-map] (partition-all 2 proto+maps)
           :let [protocol-ns (some-> protocol-var :var meta :ns str)]
           ;; TODO: maybe an exception or a log instead of silently ignoring
           :when (and protocol-ns (= :map (:op method-map)))]
       ;; 2. Inner loop -- each k/v pair in the function map
       (for [[fn-kw body] (map vector (:keys method-map) (:vals method-map))
             ;; For each function:
             ;; - ns is the protocol's namespace
             ;; - name is the key in the function map
             ;; - the class (to dispatch on) is the type that is being extended
             ;; - body is the value in the function map
             :let [protocol-fn (some->> fn-kw :val name (symbol protocol-ns))]
             :when protocol-fn]
         ;; similar to :method, but extend doesn't actually _add_ methods to a
         ;; type, it just adds _dispatches_ to the protocol method
         (list
          {:method {:symbol protocol-fn
                    :dispatch class-sym
                    :location (ast->location ast)}
           :children [body]}
          ;; call the dispatch from the main fn
          {:edge {:caller {:symbol protocol-fn}
                  :call {:symbol protocol-fn
                         :dispatch class-sym}
                  :location (ast->location ast)}}))))))

;; -- calls

(defmethod parse-one* :var
  [ast]
  ;; clojure function invocations and function arguments
  {:call {:symbol (util/var->symbol (:var ast))
          :location (ast->location ast)}})

(defmethod parse-one* :the-var
  [ast]
  ;; a (var) form
  {:call {:symbol (util/var->symbol (:var ast))
          :location (ast->location ast)}})

;; Much of this code copied or adapted from
;; https://github.com/clojure/clojure/blob/clojure-1.10.0/src/clj/clojure/core_deftype.clj#L535
(defn super-chain [^Class c]
  (when c
    (cons c (super-chain (.getSuperclass c)))))

(defn protocol-dispatch
  "Finds the class that implements the given protocol."
  [protocol cls]
  (def cls cls)
  (if (isa? cls (:on-interface protocol))
    ;; If this is from a deftype or defrecord, the class _is_ the implementer
    cls
    ;; Otherwise search through superclasses
    (and cls (->> (super-chain (box cls))
                  (filter (partial get (:impls protocol)))
                  first))))

(defmethod parse-one* :protocol-invoke
  [{:keys [protocol-fn target args] :as ast}]
  ;; protocol function invocations
  (let [protocol (-> protocol-fn :meta :protocol var-get)
        interface (:on-interface protocol)
        protocol-fn (util/var->symbol (:var protocol-fn))
        cls (parse-class/ast-class target)
        call (if (isa? cls interface)
               ;; class is an implementer -- use a class method
               {:symbol (symbol (.getName cls) (munge (name protocol-fn)))}
               ;; class is not an implementer -- use the raw protocol function,
               ;; if we have a class, attempt to find a dispatch value
               (let [dispatch (protocol-dispatch protocol cls)]
                 (cond-> {:symbol protocol-fn}
                   dispatch (assoc :dispatch dispatch))))]
    {:call (assoc call :location (ast->location ast))
     :children (cons target args)}))

(defmethod parse-one* :instance-call
  [{:keys [instance method args] :as ast}]
  ;; java method invocations
  (let [cls (or (parse-class/ast-class instance)
                (parse-class/ast-class ast)
                Object)]
    {:call {:symbol (symbol (.getName cls) (str method)) ;; already munged
            :location (ast->location ast)}
     :children (cons instance args)}))

(defmethod parse-one* :host-interop
  [{:keys [m-or-f] :as ast}]
  ;; By definition, this op-code means we have no type information, and that
  ;; this is either a field, or a 0-argument method, so there are no children
  {:call {:symbol (symbol (.getName Object) (str m-or-f))}})

(defmethod parse-one* :static-call
  [{:keys [method args] :as ast}]
  ;; java static method invocations
  (let [cls (or (parse-class/ast-class ast) Object)]
    {:call {:symbol (symbol (.getName cls) (str method)) ;; already munged
            :location (ast->location ast)}
     :children args}))

(defmethod parse-one* :new
  [{:keys [tag args] :as ast}]
  ;; java constructors
  {:call {:symbol (symbol (str (.getName tag) "."))
          :location (ast->location ast)}
   :children args})


(declare ast->graph*)

(defn make-edge [caller call]
  (if caller
    {:caller (select-keys caller [:symbol :dispatch])
     :call (select-keys call [:symbol :dispatch])
     :location (:location call)}
    ;; top-level edges (without a parent) use the current namespace
    (when-let [ns-sym (get-in call [:location :ns])]
      (make-edge {:symbol ns-sym} call))))

(defn merge-part
  [parent graph {:keys [method call edge children def]}]
  (reduce (partial ast->graph* (or def method parent))
          (cond-> graph
            def (update :nodes conj def)
            method (update :nodes conj method)
            call (update :edges conj (make-edge parent call))
            edge (update :edges conj edge)
            ;; def inside another form counts as an edge
            ;; NB: we don't want to create an edge for every top-level `def`,
            ;; hence the check on `parent` as well
            (and def parent) (update :edges conj (make-edge parent def)))
          children))

(defn ast->graph*
  [parent graph ast]
  (reduce (partial merge-part parent) graph (parse-one ast)))

(defn ast->graph'
  "Parses a full AST into a map of :nodes and :edges.

  Each symbol represents a node in the graph. Nodes are maps with keys:

    :symbol   -- the var that was defined (as a fully-qualified symbol)
    :dispatch -- the dispatch value (for multimethods and types/records)
    :location -- where the var was defined. a map of :ns, :file, :line

  Each call represents an edge in the graph. Edges are maps with keys:

    :caller   -- the calling function; a map of :symbol and :dispatch
    :call     -- the function that was called; a map with :symbol
    :location -- where the call happened (map of :ns, :file, :line)"
  [ast]
  (util/merge-graphs
   (ast->graph* nil {:nodes [] :edges []} ast)
   (keep ast-meta->graph (ana.ast/nodes ast))))

(defn ns->graph'
  "Returns a call graph for a namespace symbol."
  [sym]
  (log/warnf "parsing namespace: %s" sym)
  (->> sym
       (ana.jvm/analyze-ns)
       (map ast->graph')
       util/merge-graphs))
)
