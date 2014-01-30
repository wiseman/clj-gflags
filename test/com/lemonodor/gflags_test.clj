(ns com.lemonodor.gflags-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer :all]
   [com.lemonodor.gflags :as gflags]))


(deftest string-test
  (testing "string long name"
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-string "filename"
        "default-filename"
        "The input filename")
      (let [args ["argv0" "--filename" "foo" "arg1"]
            unparsed-args (gflags/parse-flags args)]
        (is (= unparsed-args ["arg1"]))
        (let [flags (gflags/flags)]
          (is (contains? flags :filename))
          (is (not (contains? flags :unknown-flag)))
          (is (= (flags :filename) "foo"))))))
  (testing "string short name"
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-string "filename"
        "default-filename"
        "The input filename"
        :short-name "f")
      (let [args ["argv0" "-f" "foo" "arg1"]
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
  (testing "long implicit true boolean flag"
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
          (is (flags :enable-unicorns))))))
  (testing "long implicit false boolean flag"
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-boolean "enable-unicorns"
        false
        "Whether we should enable unicorns"
        :short-name "u")
      (let [args ["argv0" "--noenable-unicorns" "arg1"]
            unparsed-args (gflags/parse-flags args)]
        (is (= unparsed-args ["arg1"]))
        (let [flags (gflags/flags)]
          (is (contains? flags :enable-unicorns))
          (is (not (contains? flags :unknown-flag)))
          (is (not (flags :enable-unicorns)))))))
  (testing "long explicit true boolean flag"
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-boolean "enable-unicorns"
        false
        "Whether we should enable unicorns"
        :short-name "u")
      (let [args ["argv0" "--enable-unicorns=true" "arg1"]
            unparsed-args (gflags/parse-flags args)]
        (is (= unparsed-args ["arg1"]))
        (let [flags (gflags/flags)]
          (is (contains? flags :enable-unicorns))
          (is (not (contains? flags :unknown-flag)))
          (is (flags :enable-unicorns))))))
  (testing "long explicit false boolean flag"
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-boolean "enable-unicorns"
        false
        "Whether we should enable unicorns"
        :short-name "u")
      (let [args ["argv0" "--enable-unicorns=false" "arg1"]
            unparsed-args (gflags/parse-flags args)]
        (is (= unparsed-args ["arg1"]))
        (let [flags (gflags/flags)]
          (is (contains? flags :enable-unicorns))
          (is (not (contains? flags :unknown-flag)))
          (is (not (flags :enable-unicorns)))))))
  (testing "short implicit true boolean flag"
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-boolean "enable-unicorns"
        false
        "Whether we should enable unicorns"
        :short-name "u")
      (let [args ["argv0" "-u" "arg1"]
            unparsed-args (gflags/parse-flags args)]
        (is (= unparsed-args ["arg1"]))
        (let [flags (gflags/flags)]
          (is (contains? flags :enable-unicorns))
          (is (flags :enable-unicorns))))))
  (testing "bad boolean flag value"
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-boolean "enable-unicorns"
        false
        "Whether we should enable unicorns"
        :short-name "u")
      (let [args ["argv0" "--enable-unicorns=red" "arg1"]]
        (is (thrown-with-msg?
             Exception #"enable-unicorns.*boolean.*red"
             (gflags/parse-flags args)))))))


(deftest integer-test
  (testing "long valid integer flag with ="
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-integer "num-unicorns"
        0
        "The number of unicorns."
        :short-name "u")
      (let [args ["argv0" "--num-unicorns=1" "arg1"]
            unparsed-args (gflags/parse-flags args)]
        (is (= unparsed-args ["arg1"]))
        (let [flags (gflags/flags)]
          (is (contains? flags :num-unicorns))
          (is (= (flags :num-unicorns) 1))))))
  (testing "long valid integer flag without ="
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-integer "num-unicorns"
        0
        "The number of unicorns."
        :short-name "u")
      (let [args ["argv0" "--num-unicorns" "1" "arg1"]
            unparsed-args (gflags/parse-flags args)]
        (is (= unparsed-args ["arg1"]))
        (let [flags (gflags/flags)]
          (is (contains? flags :num-unicorns))
          (is (= (flags :num-unicorns) 1))))))
  (testing "bad integer flag value"
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-integer "num-unicorns"
        1
        "The number of unicorns"
        :short-name "u")
      (let [args ["argv0" "--num-unicorns=red" "arg1"]]
        (is (thrown-with-msg?
             Exception #"num-unicorns.*integer.*red"
             (gflags/parse-flags args)))))))


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


;; (deftest flagfile-test
;;   (testing "reading a flag file"
;;     (binding [gflags/*flags* (gflags/make-flag-values)]
;;       (gflags/define-boolean "enable-unicorns"
;;         false
;;         "Whether we should enable unicorns"
;;         :short-name "u")
;;       (gflags/define-string "filename"
;;         "default-filename"
;;         "The input filename")
;;       (let [args ["argv0"
;;                   "--flagfile"
;;                   (str (io/file (io/resource "test.flags")))
;;                   "arg1"]
;;             unparsed-args (gflags/parse-flags args)]
;;         (is (= unparsed-args ["arg1"]))
;;         (let [flags (gflags/flags)]
;;           (is (contains? flags :enable-unicorns))
;;           (is (flags :enable-unicorns))
;;           (is (contains? flags :filename))
;;           (is (= (flags :filename) "dir/file")))))))
