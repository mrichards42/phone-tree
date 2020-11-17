(ns phone-tree.core
  (:require [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [phone-tree.graph :as graph]
            [phone-tree.html :as html]
            [phone-tree.log :as log]
            [phone-tree.namespace :refer [nss-in-dirs prepare-namespaces]]
            [phone-tree.parse :as parse]
            [ubergraph.core :as uber]))

;;; Command line parsing

(defn collect-val [existing v]
  (if (::replace (meta existing))
    (recur (vary-meta (empty existing) dissoc ::replace) v)
    (if (map? v)
      (merge existing v)
      (conj (or existing []) v))))

(defn collect-fn [m k v]
  (update m k collect-val v))

;; TODO: these are probably not exactly right
(def exclude-clojure-regex #"clojure\.(core/|lang\.).*")
(def exclude-java-regex #"java\.(lang\.|util\.).*")

;; TODO: allow parsing an analysis file once and doing the pruning later
(def cli-options
  [;; Output options
   ["-T" "--format TYPE"
    "Output format (edn, dot, html, csv)"
    :default :edn
    :parse-fn keyword
    :validate-fn [#{:edn :dot :html :csv}]]
   [nil "--dot-attr KEY=VALUE"
    "Global dot output options (can be repeated)."
    :default ^::replace {:rankdir "LR"}
    :parse-fn (fn [s]
                (->> (str/split s #"\s*;\s*")
                     (map #(str/split % #"\s*=\s*" 2))
                     (into {})))
    :assoc-fn collect-fn]

   ;; Input options
   [nil "--graph FILE.edn"
    "Import an existing edn graph file instead of analyzing namespaces"]

   ;; Graph building options
   [nil "--include-ns REGEX"
    "Project namespaces to analyze (can be repeated)."
    :parse-fn re-pattern
    :assoc-fn collect-fn]
   [nil "--exclude-ns REGEX"
    "Project namespaces to exclude from analysis (can be repeated)."
    :parse-fn re-pattern
    :assoc-fn collect-fn]

   ;; Graph pruning options
   ;; TODO: might remove --include-clojure and --include-java for now?
   [nil "--include-clojure"
    "Include clojure.core and clojure.lang nodes in the graph."
    :default false]
   [nil "--include-java"
    "Include java.lang and java.util nodes in the graph."
    :default false]
   [nil "--exclude-node REGEX"
    "Exclude these nodes from the graph (can be repeated)."
    :parse-fn re-pattern
    :assoc-fn collect-fn]
   [nil "--include-node REGEX"
    "Include only these nodes in the graph (can be repeated)."
    :parse-fn re-pattern
    :assoc-fn collect-fn]
   [nil "--include-descendants REGEX"
    "Include only descendants of these nodes in the graph (can be repeated)."
    :parse-fn re-pattern
    :assoc-fn collect-fn]
   [nil "--include-ancestors REGEX"
    "Include only ancestors of these nodes in the graph (can be repeated)."
    :parse-fn re-pattern
    :assoc-fn collect-fn]
   [nil "--collapse REGEX"
    "Collapse intermediate edges between these nodes (can be repeated)."
    :parse-fn re-pattern
    :assoc-fn collect-fn]

   ;; Misc options
   ["-v" nil
    "Set verbosity (repeat to increase verbosity)"
    :id :log-level
    :assoc-fn (fn [m k _]
                (update m k #(case %
                               (nil, :warn) :debug
                               :debug :trace
                               :trace)))]
   ["-h" "--help"
    "Show this help messsage"]
   ])

(defn usage [options-summary]
  (->> ["Usage: lein phone-tree [options] [source-paths]"
        ""
        "Options:"
        options-summary
        ""]
       (str/join "\n")))

(defn conform-options [opts args]
  (cond-> opts
    (seq args) (assoc :source-paths args)
    (not (:include-clojure opts)) (update :exclude-node (fnil conj []) exclude-clojure-regex)
    (not (:include-java opts)) (update :exclude-node (fnil conj []) exclude-java-regex)))

(defn parse-cli [args]
  (let [{:keys [options arguments errors summary]} (cli/parse-opts args cli-options)]
    (cond
      (:help options)
      {:exit 0, :message (usage summary)}
      errors
      {:exit 1, :message (str/join "\n" errors)}
      :else
      (conform-options options arguments))))

;;; Main

(defn matches-any? [regexes s]
  (seq (filter #(re-matches % s) regexes)))

(defn find-namespaces [{:keys [source-paths include-ns exclude-ns]}]
  (cond->> (nss-in-dirs source-paths)
    true (map str)
    (seq include-ns) (filter (partial matches-any? include-ns))
    (seq exclude-ns) (remove (partial matches-any? exclude-ns))
    true (map symbol)))

(defn prune-graph [g opts]
  (if-let [collapse (:collapse opts)]
    ;; collapse is a special case
    (graph/collapse g (partial matches-any? collapse))
    ;; otherwise prune
    (let [{:keys [include-node exclude-node]} opts
          {:keys [include-ancestors include-descendants]} opts]
      (cond-> g
        (seq include-node) (graph/filter-nodes (partial matches-any? include-node))
        (seq exclude-node) (graph/remove-nodes (partial matches-any? exclude-node))
        (seq include-ancestors) (graph/filter-ancestors (partial matches-any? include-ancestors))
        (seq include-descendants) (graph/filter-descendants (partial matches-any? include-descendants))))))

(defn print-graph [g opts]
  (case (:format opts)
    :edn (prn (uber/ubergraph->edn g))
    :dot (println (graph/->dot g (:dot-attr opts)))
    :html (println (html/render (graph/->json g)))
    :csv (println (graph/->csv g))))

(defn run [opts]
  (binding [log/*level* (:log-level opts :warn)]
    (log/debugf "parsed command line %s" opts)
    (when-let [exit-code (:exit opts)]
      (some-> opts :message println)
      (System/exit exit-code))
    (if (:graph opts)
      (-> (slurp (:graph opts))
          (clojure.edn/read-string)
          (uber/edn->ubergraph)
          (prune-graph opts)
          (print-graph opts))
      (-> (find-namespaces opts)
          (prepare-namespaces)
          (parse/nss->graph)
          (prune-graph opts)
          (print-graph opts)))))

(defn run-with-project [project-opts & args]
  (run (merge project-opts (parse-cli args))))

(defn -main [& args]
  (run (parse-cli args)))

(comment
  ;; generate our own call graph
  (let [opts (parse-cli ["src" "-Tedn" "-v"])]
    (if (:exit opts)
      opts
      (def G (uber/edn->ubergraph (clojure.edn/read-string (with-out-str (run opts)))))))
  )
