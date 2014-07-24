(ns com.lemonodor.gflags-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer :all]
   [com.lemonodor.gflags :as gflags]))


(deftest flags-test
  (testing "flags with no arguments"
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-string "filename"
        "default-filename"
        "The input filename")
      (let [args ["argv0" "--filename" "foo" "arg1"]
            unparsed-args (gflags/parse-flags args)]
        (is (= unparsed-args ["arg1"]))
        (let [flags (gflags/flags)]
          (is (map? flags))
          (is (contains? flags :filename))))))
  (testing "flags with an argument"
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-string "filename"
        "default-filename"
        "The input filename")
      (let [args ["argv0" "--filename" "foo" "arg1"]
            unparsed-args (gflags/parse-flags args)]
        (is (= unparsed-args ["arg1"]))
        (is (= (gflags/flags :filename) "foo"))))))


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
  (testing "string default value"
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
          (is (= (flags :filename) "default-filename"))))))
  (testing "string long name, no value"
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-string "filename"
        "default-filename"
        "The input filename"
        :short-name "f")
      (is (thrown-with-msg?
           Exception #"--filename.*requires an argument"
           (gflags/parse-flags ["argv0" "--filename"])))))
  (testing "string short name, no value"
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-string "filename"
        "default-filename"
        "The input filename"
        :short-name "f")
      (is (thrown-with-msg?
           Exception #"-f.*requires argument"
           (gflags/parse-flags ["argv0" "-f"]))))))


(deftest boolean-test
  (testing "boolean, long name, implicit true"
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
  (testing "boolean, long name, implicit false"
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
  (testing "boolean, long name, explicit true"
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
  (testing "boolean, long name, explicit false"
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
  (testing "boolean, short name, implicit true"
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
  (testing "boolean, long name, explicit illegal value"
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
  (testing "integer, long name, ="
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
  (testing "integer, long name, without ="
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
  (testing "integer, short name"
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-integer "num-unicorns"
        0
        "The number of unicorns."
        :short-name "u")
      (let [args ["argv0" "-u" "1" "arg1"]
            unparsed-args (gflags/parse-flags args)]
        (is (= unparsed-args ["arg1"]))
        (let [flags (gflags/flags)]
          (is (contains? flags :num-unicorns))
          (is (= (flags :num-unicorns) 1))))))
  (testing "integer, long name, bad value"
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-integer "num-unicorns"
        1
        "The number of unicorns"
        :short-name "u")
      (let [args ["argv0" "--num-unicorns=red" "arg1"]]
        (is (thrown-with-msg?
             Exception #"--num-unicorns.*integer.*red"
             (gflags/parse-flags args))))))
  (testing "integer, long name, no value"
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-integer "num-unicorns"
        1
        "The number of unicorns"
        :short-name "u")
      (let [args ["argv0" "--num-unicorns"]]
        (is (thrown-with-msg?
             Exception #"--num-unicorns.*requires an argument"
             (gflags/parse-flags args)))))))


(deftest float-test
  (testing "float, long name, ="
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-float "fraction"
        0
        "The fraction to use."
        :short-name "f")
      (let [args ["argv0" "--fraction=1.5" "arg1"]
            unparsed-args (gflags/parse-flags args)]
        (is (= unparsed-args ["arg1"]))
        (let [flags (gflags/flags)]
          (is (contains? flags :fraction))
          (is (= (flags :fraction) 1.5))))))
  (testing "float, long name, without ="
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-float "fraction"
        0
        "The fraction to use."
        :short-name "f")
      (let [args ["argv0" "--fraction" "1.5" "arg1"]
            unparsed-args (gflags/parse-flags args)]
        (is (= unparsed-args ["arg1"]))
        (let [flags (gflags/flags)]
          (is (contains? flags :fraction))
          (is (= (flags :fraction) 1.5))))))
  (testing "float, short name"
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-float "fraction"
        0
        "The fraction to use."
        :short-name "f")
      (let [args ["argv0" "-f" "1.5" "arg1"]
            unparsed-args (gflags/parse-flags args)]
        (is (= unparsed-args ["arg1"]))
        (let [flags (gflags/flags)]
          (is (contains? flags :fraction))
          (is (= (flags :fraction) 1.5))))))
  (testing "float, long name, bad value"
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-float "fraction"
        1
        "The fraction to use"
        :short-name "f")
      (let [args ["argv0" "--fraction=red" "arg1"]]
        (is (thrown-with-msg?
             Exception #"--fraction.*float.*red"
             (gflags/parse-flags args))))))
  (testing "float, long name, no value"
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-float "fraction"
        1
        "The fraction to use"
        :short-name "f")
      (let [args ["argv0" "--fraction"]]
        (is (thrown-with-msg?
             Exception #"--fraction.*requires an argument"
             (gflags/parse-flags args)))))))


(deftest enum-test
  (testing "enum, long name, ="
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-enum "size"
        "small"
        ["small" "medium" "large"]
        "The fraction to use."
        :short-name "s")
      (let [args ["argv0" "--size=medium" "arg1"]
            unparsed-args (gflags/parse-flags args)]
        (is (= unparsed-args ["arg1"]))
        (is (= (gflags/flags :size) "medium")))))
  (testing "enum, illegal value"
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-enum "size"
        "small"
        ["small" "medium" "large"]
        "The fraction to use."
        :short-name "s")
      (let [args ["argv0" "--size=xlarge" "arg1"]]
        (is (thrown-with-msg?
             Exception #"--size.*Value should be one of"
             (gflags/parse-flags args)))))))


(deftest multi-string-test
  (testing "multi-string, long name, one value"
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-multi-string "file"
        []
        "Files to open")
      (let [args ["argv0" "--file=file1" "arg1"]
            unparsed-args (gflags/parse-flags args)]
        (is (= unparsed-args ["arg1"]))
        (is (= (gflags/flags :file) ["file1"])))))
  (testing "multi-string, long name, two values"
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-multi-string "file"
        []
        "Files to open")
      (let [args ["argv0" "--file=file1" "--file" "file2" "arg1"]
            unparsed-args (gflags/parse-flags args)]
        (is (= unparsed-args ["arg1"]))
        (is (= (gflags/flags :file) ["file1" "file2"]))))))


(deftest multi-integer-test
  (testing "multi-integer, long name, one value"
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-multi-integer "num"
        []
        "numbers")
      (let [args ["argv0" "--num=5" "arg1"]
            unparsed-args (gflags/parse-flags args)]
        (is (= unparsed-args ["arg1"]))
        (is (= (gflags/flags :num) [5])))))
  (testing "multi-integer, long name, two values"
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-multi-integer "num"
        []
        "numbers")
      (let [args ["argv0" "--num=5" "--num" "9" "arg1"]
            unparsed-args (gflags/parse-flags args)]
        (is (= unparsed-args ["arg1"]))
        (is (= (gflags/flags :num) [5 9]))))))


(deftest multi-float-test
  (testing "multi-float, long name, one value"
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-multi-float "ratio"
        []
        "ratios")
      (let [args ["argv0" "--ratio=5.3" "arg1"]
            unparsed-args (gflags/parse-flags args)]
        (is (= unparsed-args ["arg1"]))
        (is (= (gflags/flags :ratio) [5.3])))))
  (testing "multi-float, long name, two values"
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-multi-float "ratio"
        []
        "ratios")
      (let [args ["argv0" "--ratio=5.3" "--ratio" "9.1" "arg1"]
            unparsed-args (gflags/parse-flags args)]
        (is (= unparsed-args ["arg1"]))
        (is (= (gflags/flags :ratio) [5.3 9.1]))))))


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


(testing "re-defining the same flag in the same namespace is OK"
    (let [ns3 (create-ns 'com.lemonodor.gflags-test.ns3)]
      (binding [gflags/*flags* (gflags/make-flag-values)]
        (binding [*ns* ns3]
          (gflags/define-string "filename"
            "default-filename"
            "The input filename")
          (gflags/define-string "filename"
            "default-filename"
            "The input filename")))))


(deftest flagfile-test
  (testing "reading a flag file"
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (gflags/define-boolean "enable-unicorns"
        false
        "Whether we should enable unicorns")
      (gflags/define-boolean "enable-dragons"
        false
        "Whether we should enable dragons")
      (gflags/define-integer "num-cats"
        2
        "The number of cats to pet")
      (gflags/define-string "best-cat-name"
        "mr. shrimp"
        "The best cat name")
      (gflags/define-float "dog-years-multiplier"
        7
        "person years per dog year")
      (gflags/define-string "foo"
        nil
        "Just a foo flag.")
      (let [args ["argv0"
                  "--flagfile"
                  (str (io/file (io/resource "test.flags")))
                  "arg1"]
            unparsed-args (gflags/parse-flags args)]
        (is (= unparsed-args ["arg1"]))
        (is (= (set (keys (gflags/flags)))
               #{
                 :best-cat-name
                 :dog-years-multiplier
                 :enable-dragons
                 :enable-unicorns
                 :flagfile
                 :foo
                 :?
                 :help
                 :num-cats
                 }))
        (is (= (gflags/flags :best-cat-name) "shrimp"))
        (is (= (gflags/flags :dog-years-multiplier) 6.9))
        (is (gflags/flags :enable-unicorns))
        (is (gflags/flags :enable-dragons))
        (is (= (gflags/flags :foo) "bar"))
        (is (= (gflags/flags :num-cats) 8))))))

(deftest help-test
  (testing "usage with --help"
    (binding [gflags/*flags* (gflags/make-flag-values)]
      (let [args ["argv0" "--help"]]
        (gflags/parse-flags args)))))
