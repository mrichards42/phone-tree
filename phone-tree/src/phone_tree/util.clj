(ns phone-tree.util)

(defn var->symbol [v]
  (symbol (some-> v meta :ns str)
          (some-> v meta :name str)))
