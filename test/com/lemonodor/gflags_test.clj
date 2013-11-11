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


(deftest boolean-test
  (testing "boolean flag"
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-boolean "enable-unicorns"
        false
        "Whether we should enable unicorns"
        :short-name "u")
      (let [args ["argv0" "--enable-unicorns" "arg1"]
            unparsed-args (gflags/parse-flags args)]
        (is (= unparsed-args ["arg1"]))
        (let [flags (gflags/flags)]
          (is (contains? flags :enable-unicorns))
          (is (not (contains? flags :unknown-flag)))
          (is (flags :enable-unicorns)))))))


(deftest redefinition-test
  (testing "colliding definitions throw errors"
    (let [ns1 (create-ns 'com.lemonodor.gflags-test.ns1)
          ns2 (create-ns 'com.lemonodor.gflags-test.ns2)]
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (binding [*ns* ns1]
        (gflags/define-string "filename"
          "default-filename"
          "The input filename"
          :short-name "f"))
      (is (thrown-with-msg?
           Exception #"flag.*conflicts"
           (binding [*ns* ns2]
             (gflags/define-string "filename"
               "default-filename"
               "The input filename"
               :short-name "f"))))))))
