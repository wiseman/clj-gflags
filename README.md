# clj-gflags

[![Build Status](https://travis-ci.org/wiseman/clj-gflags.png?branch=master)](https://travis-ci.org/wiseman/clj-gflags)

Google flags ("gflags") for Clojure. See
https://code.google.com/p/python-gflags/ and
https://github.com/schuhschuh/gflags.

The Google approach to command-line flags is that they are defined in
the source file in which they are used.  This means that in a
situation with lots of code sharing, where you may have 3 or 4
command-line apps that all use one common module, the common module
can define its own command line flags, which then get used by each
app.

It may sound a little weird, but in practice it works well and it very
convenient.

In this translation of the concept to clojure, each namespace can
define its own flags.


## Installation

```
[com.lemonodor/gflags "0.2.0"]
```


## Example usage

Say you have a namespace for your S3 utilities.  You can define some
flags in that namespace for your AWS credentials etc.:

```
(ns s3-utils
  "Utilties for accessing AWS S3."
  (:require [:com.lemonodor.gflags :as gflags])
  ...)

(gflags/define-string "aws-access-key"
  nil
  "The AWS access key to use.")
(gflags/define-string "aws-secret-key"
  nil
  "The AWS secret key to use.")
(gflags/define-integer "num-retries"
  3
  "The number of times to retry S3 operations.")

(defn get-s3-file [url & {:keys [access-key secret-key num-retries]}]
  (let [access-key (or access-key (gflags/flags :aws-access-key))
        secret-key (or secret-key (gflags/flags :aws-secret-key))]
    ...))
```

Then in your CLI app namespace, you just need to call `parse-flags`:
```
(ns main-app
  "Command-line app"
  (:require [:com.lemonodor.gflags :as gflags])
  ...)

(defn- main [& args]
  (let [unparsed-args (gflags/parse-flags (into ["argv0"] args))]
    ...))
```
