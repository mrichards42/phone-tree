(ns phone-tree.sample.simple-ns
  "A test namespace with no requires")

(def x 10)

(defn y [n] (* n x))

(defmulti numeric-multi y)

(defmethod numeric-multi 10
  [_]
  :ten)

(defmethod numeric-multi 100
  [_]
  (y 10))

(defmethod numeric-multi :default
  [_]
  :nope)

(defprotocol StringProtocol
  (join [this coll]))

(defrecord StringRecord [sep]
  StringProtocol
  (join [this coll]
    (apply str (interpose sep coll))))

(deftype StringType [sep]
  StringProtocol
  (join [this coll]
    (apply str (interpose sep coll))))

(def string-reify
  (reify StringProtocol
    (join [this coll]
      (apply str (interpose "," coll)))))

(defn -main []
  (println "x =>" x)
  (println "(y 5) =>" (y 5))
  (println "(numeric-multi 1) =>" (numeric-multi 1))
  (println "(numeric-multi 10) =>" (numeric-multi 10))
  (println "(numeric-multi 5) =>" (numeric-multi 5))
  (println "(join (->StringRecord \"--\") [1 2 3]) =>" (join (->StringRecord "--") [1 2 3]))
  (println "(join (->StringType \"/\") [1 2 3]) =>" (join (->StringType "/") [1 2 3]))
  (println "(join string-reify [1 2 3]) =>" (join string-reify [1 2 3]))
  )
