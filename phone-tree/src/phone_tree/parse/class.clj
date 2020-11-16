(ns phone-tree.parse.class
  "Class guessing."
  (:require [phone-tree.log :as log]
            [phone-tree.parse.ast :as ast]))


;;; Reify hack

;; reify creates and instantiates an anonymous class. Each instance is a new
;; class, but unlike defrecord or deftype, it is impossible to know the class
;; name during analysis, since the class is dynamically created.

;; tools.analyzer.jvm gets around this by creating a synthetic class name for
;; reify special forms. We treat this as a "template" that the real classes can
;; "inherit" from (though of course this isn't actual inheritance).

;; every form has :line and :column metadata, so we use this metadata and
;; the prefix of the reified class name as keys in this map

(def reify-templates (atom {}))

(defn reify-prefix
  "Returns the prefix for a reify class.

  Reify classes are inner classes of the namespace (and optionally, the
  function) they are defined in. e.g.:

    my.ns$reify__123456
    my.ns$my_fn$reify__123456

  The prefix is everything up to and including \"reify\""
  [cls]
  (some->> cls .getName (re-find #"^.*\$reify")))

(defn record-reify!
  "Records this reify form in `reify-templates`

  Called during analysis of reify forms."
  [cls metadata]
  (let [prefix (reify-prefix cls)]
    (when (and prefix metadata)
      (swap! reify-templates assoc [prefix metadata] cls))))

(defn reify-class
  "Returns the (synthetic) class for a var with a reified class."
  [v]
  (let [prefix (some->> v class reify-prefix)
        metadata (meta v)]
    (get @reify-templates [prefix metadata])))

(defmethod ast/parse :reify
  [ast]
  (record-reify! (:class-name ast) (meta (:form ast)))
  ;; call the default method
  ((get-method ast/parse :default) ast))

;;; Class guessing

(defmulti guess-class* :op)

(defmethod guess-class* :default
  [_]
  nil)

(defmethod guess-class* :var
  [ast]
  (let [v (var-get (:var ast))]
    (or (reify-class v)
        (class v))))

(defmethod guess-class* :const
  [ast]
  (if (= :class (:type ast))
    (:val ast) ;; TODO: there might be exceptions to this, e.g. record or type?
    (do
      (log/debugf "guess-class with :const %s node: %s" (:type ast) ast)
      (class (:val ast)))))

(defn guess-class
  "Returns the best guess return class for this ast node.

  Ignores java.lang.Object"
  [ast]
  ;; TODO: I think :class is not necessarily correct here: might need to be
  ;; (guess-class (:class ast))?
  #_(prn "guess-class"
       (:form ast)
       (select-keys (:env ast) [:file :line])
       [(guess-class* ast)
        (:class ast)
        (:o-tag ast)
        (:tag ast)])
  (->> (keep #(% ast) [guess-class* :class :o-tag :tag])
       (keep #(cond
                (class? %) %
                (map? %) (guess-class %)))
       (remove #{java.lang.Object})
       first))
