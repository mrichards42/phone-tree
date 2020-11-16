(ns phone-tree.redef
  "Functions that provide macro redefs."
  (:require [clojure.java.io :as io]
            [phone-tree.log :as log]))

(defn read-forms
  "Reads all forms in a file."
  [filename]
  (with-open [r (io/reader filename)]
    (let [pbr (java.io.PushbackReader. r)]
      (doall
        (take-while #(not= ::EOF %)
                    (repeatedly #(read {:eof ::EOF} pbr)))))))

(defn try-require [& args]
  (try
    (apply require args)
    true
    (catch java.io.FileNotFoundException _
      false)))

(defn redef-form
  "Redefines a form. Expects a def-style form with a fully-qualified symbol.

  Ignores any redefs for namespaces that are not in the classpath."
  [[head qualified-sym & body :as form]]
  (let [sym-ns (symbol (namespace qualified-sym))
        sym-name (symbol (name qualified-sym))
        form-to-eval (cons head (cons sym-name body))]
    ;; redefs should survive `require`, since we've already required the
    ;; namespace here, but they definitely _won't_ survive :reload
    (if (try-require sym-ns)
      (binding [*ns* (find-ns sym-ns)]
        (log/debugf "Redefining %s" qualified-sym)
        (when (= 'defmulti head) (ns-unmap *ns* sym-name))
        (eval form-to-eval))
      (log/warnf "Skipping redef %s: unable to require %s" qualified-sym sym-ns))))

(defn load-redefs
  [filename]
  (log/debugf "Loading redefs from %s" filename)
  (->> (read-forms filename)
       (map redef-form)
       (filter identity)
       set))
