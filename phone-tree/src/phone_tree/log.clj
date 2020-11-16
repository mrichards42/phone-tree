(ns phone-tree.log
  "Debug logging")

(def ^:dynamic *level* :warn)

(def level-map
  {:trace #{:trace :debug :warn}
   :debug #{:debug :warn}
   :warn #{:warn}})

(defn enabled?
  [level]
  (contains? (level-map *level*) level))

(defn log
  [level & strs]
  (when (enabled? level)
    (binding [*out* *err*]
      (apply println strs))))

(def trace (partial log :trace))
(def debug (partial log :debug))
(def warn (partial log :warn))

(defn logf
  [level fmt & args]
  (when (enabled? level)
    (binding [*out* *err*]
      (apply printf fmt args)
      (println))))

(def tracef (partial logf :trace))
(def debugf (partial logf :debug))
(def warnf (partial logf :warn))
