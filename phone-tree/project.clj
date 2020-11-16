(defproject phone-tree "0.1.0-SNAPSHOT"
  :description "Call graph generator for Clojure."
  :url "https://github.com/mrichards42/phone-tree"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :main phone-tree.core

  :plugins [[thomasa/mranderson "0.5.1"]]

  :dependencies [[org.clojure/clojure "1.10.1"]
                 ^:inline-dep [org.clojure/data.csv "1.0.0"]
                 ^:inline-dep [org.clojure/data.json "1.0.0"]
                 ^:inline-dep [org.clojure/tools.analyzer "1.0.0"]
                 ^:inline-dep [org.clojure/tools.analyzer.jvm "1.0.0"]
                 ^:inline-dep [org.clojure/tools.cli "1.0.194"]
                 ^:inline-dep [org.clojure/tools.namespace "1.0.0"]
                 ;; ubergraph has a couple deps that don't work with mranderson
                 [com.rpl/specter "1.1.2"]
                 [potemkin "0.4.5"]
                 ^:inline-dep [ubergraph "0.8.2" :exclusions [com.rpl/specter
                                                              potemkin]]]

  :profiles {:dev {:dependencies [;; for testing macroexpand of go-loop
                                  [org.clojure/core.async "1.0.567"]]}})
