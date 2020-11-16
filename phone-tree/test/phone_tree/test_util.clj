(ns phone-tree.test-util
  (:require [clojure.string :as str]
            [clojure.tools.analyzer.jvm :as ana.jvm]
            [clojure.walk :as walk]
            [phone-tree.parse :as parse]
            [phone-tree.util :as util]))

(binding [*ns* (create-ns 'test-ns)]
  (refer-clojure))

(defn analyze+eval
  "Analyzes a form in the 'user namespace."
  [form]
  (binding [*ns* (create-ns 'test-ns)]
    (ana.jvm/analyze+eval form)))

(defn map-vals [f m]
  (reduce-kv (fn [m k v] (assoc m k (f v))) m m))

(defn simplify-graph
  "Simplifies a call graph.

  - no :location key
  - :nodes and :edges are sets"
  [graph]
  (->> graph
       (walk/prewalk (fn [x]
                       (cond
                         (map? x) (dissoc x :location)
                         :else x)))
       (map-vals set)))

(def simple-graph
  "Parse a form into a simplified call graph"
  (comp simplify-graph parse/ast->graph analyze+eval))

(defn subgraph?
  "Does one call graph contain the second call graph?"
  [graph sub-graph]
  (and (every? (set (:nodes graph)) (:nodes sub-graph))
       (every? (set (:edges graph)) (:edges sub-graph))))
