(ns phone-tree.html
  "HTML render using vis.js"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-html-resource [filename]
  (slurp (io/resource (str "html/" filename))))

(defn mini-hbs
  "Mini hbs renderer"
  [template context]
  (-> template
      (str/replace #"\{\{>\s*(\S+)\s*\}\}" (fn [[_ file]]
                                             (read-html-resource file)))
      (str/replace #"\{\{\s*(\S+)\s*\}\}" (fn [[_ k]]
                                            (get context (keyword k))))))

(defn render [json-graph]
  (mini-hbs (read-html-resource "template.hbs")
            {:json-graph json-graph}))
