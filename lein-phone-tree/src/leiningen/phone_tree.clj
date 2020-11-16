(ns leiningen.phone-tree
  (:require [leiningen.core.eval :refer [eval-in-project]]
            [leiningen.core.project :refer [merge-profiles]]))

(defn ^:pass-through-help phone-tree
  [project & args]
  (let [project-opts (select-keys project [:source-paths])
        deps {:dependencies [['phone-tree "0.1.0-SNAPSHOT"]]}]
    (eval-in-project
     (merge-profiles project [deps])
     `(phone-tree.core/run-with-project '~project-opts ~@args)
     '(require 'phone-tree.core))))
