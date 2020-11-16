(ns phone-tree.graph
  "Graph manipulation functions"
  (:require [clojure.data.csv :as csv]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [loom.alg-generic :as loom.alg]
            [loom.graph :as lg]
            [loom.attr :as la]
            [ubergraph.core :as uber]))

;; Loom shortcuts

(defn merge-attrs [g node-or-edge attrs]
  (reduce-kv (fn [g k v] (la/add-attr g node-or-edge k v))
             g
             attrs))

(defn node-with-attrs [g n]
  [n (or (la/attrs g n) {})])

(defn edge-with-attrs [g edge]
  [(lg/src edge) (lg/dest edge) (or (la/attrs g edge) {})])

(defn digraph
  "loom.graph/digraph wrapper that allows nodes and edges to have attrs"
  [& nodes-and-edges]
  (reduce (fn [g init]
            (cond
              (not (sequential? init)) (lg/add-nodes g init)
              (map? (second init)) (-> g
                                       (lg/add-nodes (first init))
                                       (merge-attrs (first init) (second init)))
              :else (-> g
                        (lg/add-edges (take 2 init))
                        (merge-attrs (vec (take 2 init)) (nth init 2 nil)))))
          (lg/digraph)
          nodes-and-edges))


;;; Graph manipulation

(defn remove-nodes
  "Removes nodes and edges given a function `f` that takes a node id.

  Edges will be removed if either :src or :dest matches."
  [g f]
  (apply uber/remove-nodes g (filter f (uber/nodes g))))

(defn filter-nodes
  "Filters nodes and edges given a function `f` that takes a node id.

  Edges will be included if both :src and :dest match."
  [g f]
  (remove-nodes g (complement f)))

(defn reachable-nodes
  "Returns the set of nodes that are reachable from `starting-nodes`."
  [neighbors-fn starting-nodes]
  (letfn [(neighbors [node]
            (if (= ::start node)
              starting-nodes
              (neighbors-fn node)))]
    (set (loom.alg/bf-traverse neighbors ::start))))

(defn- ->starting-nodes [g starting-nodes-or-fn]
  (if (fn? starting-nodes-or-fn)
    (filter starting-nodes-or-fn (uber/nodes g))
    starting-nodes-or-fn))

(defn filter-descendants
  "Returns a graph with nodes and edges that are descendants of `starting-nodes`
  (a coll or node filtering function)."
  [g starting-nodes-or-fn]
  (let [starting-nodes (->starting-nodes g starting-nodes-or-fn)]
    (filter-nodes g (reachable-nodes (uber/successors g) starting-nodes))))

(defn filter-ancestors
  "Returns a graph with nodes and edges that are ancestors of `starting-nodes`
  (a coll or node filtering function)."
  [g starting-nodes-or-fn]
  (let [starting-nodes (->starting-nodes g starting-nodes-or-fn)]
    (filter-nodes g (reachable-nodes (uber/predecessors g) starting-nodes))))

(defn collapse
  "Returns a graph that only contains `starting-nodes` (a coll or node
  filtering function). Intermediate edges and nodes are collapsed."
  [g starting-nodes-or-fn]
  (let [starting-nodes (set (->starting-nodes g starting-nodes-or-fn))
        edges (for [start starting-nodes
                    end (->> (reachable-nodes (uber/successors g) #{start})
                             (filter starting-nodes))
                    ;; this would include self-edges by default (since
                    ;; reachable-nodes also includes starting nodes), so don't
                    ;; include the self-edge unless it actually exists
                    :when (or (not= start end)
                              (uber/find-edge g start end))]
                [start end])
        nodes (->> (reduce into starting-nodes edges)
                   (map #(uber/node-with-attrs g %)))]
    (apply uber/digraph (concat nodes edges))))


;;; DOT

(defn sanitize-id [id]
  (str/replace id ":" "_"))

(def read-lines (memoize (comp vec str/split-lines slurp)))

(defn form [{:keys [file line end-line]}]
  (let [block (subvec (read-lines file) (dec line) end-line)]
    ;; TODO: do we care about column and end-column?
    (str/join "\n" block)))

(defn tooltip [{:keys [locations]}]
  (->> (map (fn [{:keys [file line] :as location}]
              (format "%s, line %s\n%s" file line (form location))) locations)
       (remove str/blank?)
       (str/join "\n\n")
       (not-empty)))

(defn edge-attrs [e attrs]
  {:class "edge"
   :tooltip (format "%s -> %s" (uber/src e) (uber/dest e))}
  #_(let [tt (tooltip attrs)]
    (cond-> {:class "edge"}
      tt (assoc :tooltip (str/replace (uber/escape-label tt) "\n" "\\n")))))

(defn node-attrs [_ {:keys [external? type] :as attrs}]
  (let [tt (tooltip attrs)]
    (cond-> {:class "node"
             :style "filled"
             :fillcolor "white"}
      external? (update :style str ",dashed")
      tt (assoc :tooltip (str/replace (uber/escape-label tt) "\n" "\\n"))
      ;; playing around
      type (update :class str " " (name type))
      (= :multimethod type) (assoc :fillcolor "lavender")
      (= :protocol-fn type) (assoc :fillcolor "lavender")
      (= :channel type) (assoc :fillcolor "black")
      (= :function type) (assoc :fillcolor "white")
      (= :namespace type) (assoc :fillcolor "lightpink")
      (= :macro type) (assoc :fillcolor "palegreen")
      (= :class type) (assoc :fillcolor "lightcyan")
      (= :method type) (assoc :fillcolor "lightcyan")
      (not type) (assoc :fillcolor "gray"))))

(defn with-dot-attrs
  "Returns an ubergraph with attrs suitable for dot output."
  [g]
  (let [nodes (for [n (uber/nodes g)]
                [(sanitize-id n)
                 (assoc (node-attrs n (uber/attrs g n)) :label n)])
        edges (for [e (uber/edges g)]
                [(sanitize-id (uber/src e))
                 (sanitize-id (uber/dest e))
                 (edge-attrs e (uber/attrs g e))])]
    (apply uber/digraph (concat nodes edges))))

(defn ->dot
  "Returns a dot file as a string.

  `attrs` are optional dot graph attributes."
  ([g] (->dot g nil))
  ([g attrs]
   ;; this is a little hacky: ubergraph expects to write to a file, but it uses
   ;; `spit` to write dot, so we can use a writer to get string output
   (let [writer (java.io.StringWriter.)]
     ;; force dorothy to quote ids and attributes
     (with-redefs [dorothy.core/safe-id? (constantly false)]
       (-> g
           with-dot-attrs
           (uber/viz-graph (assoc attrs :save {:format :dot :filename writer}))))
     (str writer))))


;;; JSON

(defn ->json
  "Returns a json representation of a graph.

  Notes have :id and :attrs (dot attrs)
  Edges have :src :dest and :attrs (dot attrs)"
  [g]
  (let [g (with-dot-attrs g)]
    (json/write-str {:nodes (for [n (uber/nodes g)]
                              {:id n
                               :attrs (uber/attrs g n)})
                     :edges (for [e (uber/edges g)]
                              {:src (uber/src e)
                               :dest (uber/dest e)
                               :attrs (uber/attrs g e)})})))

;;; lein-topology compatible csv

(defn ->topology
  "Transforms a graph into a lein-topology compatible format:
  a map of {[src dest] call-count}"
  [g]
  (->> (uber/edges g)
       (map (fn [e]
              [[(uber/src e) (uber/dest e)] (or (uber/attr g e :count) 1)]))
       (into {})))

(defn ->csv
  "Returns csv output"
  [g]
  (let [topology (->topology g)
        lines (map flatten topology)
        writer (java.io.StringWriter.)]
    (csv/write-csv writer lines)
    (str writer)))



;;; This code is more or less in `phone-tree.redef` and resources/redefs.edn
;; TODO: a config file for redefining macros!
;; something like this:
(comment
  (defmacro com.climate.newrelic.trace/defn-traced [name & body]
    `(defn ~name ~@body))

  (defmacro mount.core/defstate [name & body]
    `(def ~name (delay (hash-map ~@body))))

  ;; The trick is that we want to define these _after_ the namespace that
  ;; defines them is loaded, so that requiring the ns later doesn't clobber our
  ;; defs

  )
