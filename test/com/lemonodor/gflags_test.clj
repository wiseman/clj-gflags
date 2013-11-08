(ns com.lemonodor.gflags-test
  (:require [clojure.test :refer :all]
            [com.lemonodor.gflags :as gflags]))

(deftest string-test
  (testing "Oh boy"
    (gflags/define-string "filename"
      nil
      "input filename"
      :short-name "f")
    (gflags/parse-flags ["--filename=foo"])
    ;;(test (= (flags/*flags* :filename) "foo"))
    ))
