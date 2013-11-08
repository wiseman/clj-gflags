(ns com.lemonodor.gflags-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :as pprint]
            [com.lemonodor.gflags :as gflags]))

(deftest string-test
  (testing "Oh boy"
    (gflags/define-string "filename"
      "default-filename"
      "The input filename"
      :short-name "f")
    (let [args ["argv0" "--filename" "foo" "arg1"]
          unparsed-args (gflags/parse-flags args)]
      (pprint/pprint (gflags/flag-values))
      (is (= unparsed-args ["arg1"]))
      (let [flags (gflags/flags)]
        (pprint/pprint flags)
        (is (contains? flags "filename"))
        (is (not (contains? flags "unknown-flag")))
        (is (= (flags "filename") "foo"))))))
