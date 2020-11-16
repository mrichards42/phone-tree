(ns phone-tree.namespace
  "Namespace utilities"
  (:require [clojure.tools.namespace.dir :as dir]
            [clojure.tools.namespace.track :as track]))

;; borrowed from yagni: https://github.com/venantius/yagni

(defn prepare-namespaces
  "First, create all of our namespaces so that we don't have to worry about
  the order in which we load them. Then, load all of them."
  [namespaces]
  (doseq [n namespaces]
    (create-ns n))
  (doseq [n namespaces]
    (require n #_:reload))
  namespaces)

(defn nss-in-dirs
  "Return a map containing a list of all the project's namespaces."
  [dirs]
  (let [tracker (if (seq dirs)
                  (apply dir/scan-all (track/tracker) dirs)
                  (track/tracker))]
    (::track/load tracker)))
