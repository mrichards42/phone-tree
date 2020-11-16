(ns phone-tree.parse.build
  "Call graph building functions."
  (:require [phone-tree.parse.ast :as ast]
            [phone-tree.log :as log]
            [phone-tree.util :as util]
            [ubergraph.core :as uber]))

;;; Intermediate format (used by ast parsing functions)

(def ^:dynamic *parent* nil)

;; TODO: playing around w/ type guessing
(defprotocol NodeType
  (node-type [this]))

(extend-protocol NodeType
  clojure.lang.MultiFn
  (node-type [_] :multimethod)
  clojure.lang.Fn
  (node-type [_] :function)
  #_#_clojure.core.async.impl.protocols.Channel
  (node-type [_] :channel)
  java.lang.Class
  (node-type [_] :class)
  clojure.lang.Var
  (node-type [v]
    (cond
      (:macro (meta v)) :macro
      (:protocol (meta v)) :protocol-fn
      :else (node-type (var-get v))))
  clojure.lang.Symbol
  (node-type [s]
    (let [v (ns-resolve 'clojure.core s)]
      (cond
        v (node-type v)
        (find-ns s) :namespace
        (and (namespace s) (class? (ns-resolve 'clojure.core (symbol (namespace s))))) :method
        :else nil)))
  ;; Defaults
  java.lang.Object
  (node-type [_] nil)
  nil
  (node-type [_] nil))


(defn conform-node [x]
  (cond
    (not (map? x)) (recur {:symbol x})
    (var? (:symbol x)) (-> x
                           (update :symbol util/var->symbol)
                           (assoc :type (node-type (:symbol x))))
    (symbol? (:symbol x)) (assoc x :type (node-type (:symbol x)))
    (not (symbol? (:symbol x))) (throw (ex-info "Node must be a symbol or var" x))
    :else x))

(defn node-id [x]
  (if (string? x)
    x
    (let [{:keys [symbol dispatch]} (conform-node x)]
      (if dispatch
        (str symbol " " (pr-str dispatch))
        (str symbol)))))

(defn ->node [attrs]
  (assoc attrs :id (node-id attrs)))

(defn ->edge [attrs]
  (assoc attrs :id (str (:src attrs) " -> " (:dest attrs))))

(defn edge
  "Builds a graph edge, returning a sequence with a single edge."
  [ast {:keys [src dest]}]
  (if src
    (list (->edge {:src (node-id src)
                   :dest (node-id dest)
                   :location (ast/location ast)}))
    ;; make an edge from the current *parent* (or the namespace if it's a
    ;; top-level form)
    (when-let [parent (or *parent* (get-in ast [:env :ns]))]
      (log/tracef "Adding edge from *parent* %s (%s) to %s" *parent* parent dest)
      (recur ast {:src parent, :dest dest}))))

(defn node
  "Builds a graph node, returning a sequence of it and the result of parsing
  any `body-asts`."
  [ast node & body-asts]
  (let [node (-> (conform-node node)
                 (assoc :location (ast/location ast))
                 (->node))]
    (concat ;; the node itself
            (list node)
            ;; if we're inside another top-level node, add an edge to this node
            (when *parent*
              (edge ast {:src *parent* :dest (:id node)}))
            ;; parse the body with this node as the new parent
            (binding [*parent* (:id node)]
              (-> (mapcat ast/parse body-asts)
                  ;; laziness + binding = trouble, so force this sequence
                  doall))
            ;; if this ast included a macro expansion, add an edge to the macro
            (when-let [dest (and (:raw-forms ast)
                                 (ast/first-form-var ast))]
              (edge ast {:src (:id node) :dest dest})))))


;;; Intermediate format -> Ubergraph

(defn merge-attrs [attr-seq]
  (let [locations (set (keep :location attr-seq))]
    (-> (apply clojure.core/merge attr-seq)
        (dissoc :location)
        (assoc :locations locations)
        (assoc :count (count locations)))))

(defn attrs->ubergraph [attrs]
  (if (and (:src attrs) (:dest attrs))
    ;; edge
    [(:src attrs) (:dest attrs) (dissoc attrs :src :dest)]
    ;; node
    [(:id attrs) (dissoc attrs :id)]))

(defn mark-external-nodes [g]
  (->> (uber/nodes g)
       (remove (comp seq :locations (partial uber/attrs g)))
       (reduce (fn [g n] (uber/add-attr g n :external? true)) g)))

(defn ubergraph [nodes-and-edges]
  ;; collapse multiple nodes into a single node, and multiple edges into a
  ;; single edge, but retain each ast location in :locations
  (->> nodes-and-edges
       (group-by :id)
       (vals)
       (map merge-attrs)
       (map attrs->ubergraph)
       (apply uber/digraph)
       (mark-external-nodes)))
