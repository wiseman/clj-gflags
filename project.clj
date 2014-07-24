(defproject com.lemonodor/gflags "0.5.1-SNAPSHOT"
  :description "Google flags/gflags for clojure."
  :url "http://github.com/wiseman/clj-gflags/"
  :license {:name "MIT License"
            :url "http://www.opensource.org/licenses/mit-license.php"}
  :scm {:name "git"
        :url "https://github.com/wiseman/clj-gflags"}
  :dependencies [[com.lemonodor/getopt "0.1.0"]
                 [me.raynes/fs "1.4.5"]
                 [org.clojure/clojure "1.5.1"]]
  :profiles {:dev {:plugins [[lein-cloverage "1.0.2"]]}})
