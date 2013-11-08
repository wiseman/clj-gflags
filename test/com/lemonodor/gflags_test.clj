(ns com.lemonodor.gflags-test
  (:require [clojure.test :refer :all]
            ;;[clojure.pprint :as pprint]
            [com.lemonodor.gflags :as gflags]))


(deftest string-test
  (testing "string flag"
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-string "filename"
        "default-filename"
        "The input filename"
        :short-name "f")
      (let [args ["argv0" "--filename" "foo" "arg1"]
            unparsed-args (gflags/parse-flags args)]
        (is (= unparsed-args ["arg1"]))
        (let [flags (gflags/flags)]
          (is (contains? flags :filename))
          (is (not (contains? flags :unknown-flag)))
          (is (= (flags :filename) "foo"))))))
  (testing "string flag default value"
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-string "filename"
        "default-filename"
        "The input filename"
        :short-name "f")
      (let [args ["argv0" "arg1"]
            unparsed-args (gflags/parse-flags args)]
        (is (= unparsed-args ["arg1"]))
        (let [flags (gflags/flags)]
          (is (contains? flags :filename))
          (is (not (contains? flags :unknown-flag)))
          (is (= (flags :filename) "default-filename")))))))
