(ns com.lemonodor.gflags
  "Gflags for clojure."
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [com.lemonodor.getopt :as getopt]
            [me.raynes.fs :as fs]))


(defn set-flag-value [flag ^String optname value-string]
  (let [value-string (if (and (not (.startsWith optname "--"))
                              (:boolean flag))
                       "1"
                       value-string)]
    (when (not value-string)
      (throw (Exception. (str "Must specify a value for flag " optname))))
    (let [value
          (try ((:parser flag) flag value-string)
               (catch Exception e
                 (let [message (str
                                "Error while parsing value for flag " optname)
                       chained-message (.getMessage ^Throwable e)]
                   (throw
                    (Exception.
                     (if chained-message
                       (str message ": " chained-message)
                       message)
                     e)))))]
      (assoc flag :value value :present true))))


(defprotocol FlagValuesProtocol
  (register-flag [this flag ns])
  (update-flag-values [this optlist])
  (flag-map [this])
  (all-flags [this]))


(defn optname-to-flag-name [optname]
  (let [[_ name] (re-matches #"--?(.+)" optname)]
    name))


(defn flag-names [flag]
  (filter identity [(:name flag) (:short-name flag)]))


(def ^:dynamic *allow-flag-redefinition* false)


(defrecord FlagValues [__flags __flags_by_ns]
  FlagValuesProtocol
  (register-flag [this flag ns]
    ;; We set the flag's value to be its default here (but it's still
    ;; not considered present).
    (let [flag-atom (atom (assoc flag :value (:default flag)))
          flags-by-ns (:__flags_by_ns this)
          name-entries (into {} (for [name (flag-names flag)] [name flag-atom]))]
      ;; Check that a flag with this name isn't already registered.
      (when-let [conflicting-flag (some (:__flags this) (flag-names flag))]
        (when (and (not *allow-flag-redefinition*)
                   (not (= (:namespace flag) (:namespace @conflicting-flag))))
          (throw (Exception.
                  (str
                   "Flag named \""
                   (string/join
                    "\" and \"" (flag-names flag)) "\""
                    " defined in " (:namespace flag)
                    " conflicts with flag named \""
                    (string/join
                     "\" and \"" (flag-names @conflicting-flag)) "\""
                     " defined in " (:namespace @conflicting-flag))))))
      (->FlagValues (merge (:__flags this) name-entries)
                    (update-in flags-by-ns [ns] conj flag-atom))))
  (update-flag-values [this optlist]
    (doseq [[optname value-string] optlist]
      (let [name (optname-to-flag-name optname)]
        (swap! ((:__flags this) name) set-flag-value optname value-string)))
    this)
  (flag-map [this]
    (:__flags this))
  (all-flags [this]
    (distinct (for [[name flag-atom] (:__flags this)] @flag-atom))))


(def ^:dynamic *flags* nil)

(defn flag-values []
  (into {} (for [[name flag] (:__flags @*flags*)] [name flag])))


(defn flag-name-to-symbol [name]
  (keyword name))


;; Equivalent of gflags.py FLAGS.  FIXME: Should be able to return a
;; thing that acts like a map but just indexes into existing flag
;; values without actually creating a map, right?
(defn flags
  ([]
     (into
      {}
      (for [[name flag] (:__flags @*flags*)]
        [(flag-name-to-symbol name) (:value @flag)])))
  ([flag-name]
     ((flags) flag-name)))


(defrecord Flag
    [name
     default
     value
     help
     short-name
     boolean
     present
     namespace
     parser
     allow-override])


(defn define-flag [flag ns]
  (swap! *flags* register-flag flag ns))


(defn define [parser name default help & args]
  (define-flag
    (map->Flag
     (merge
      {:help "(no help available)"}
      {:parser parser
       :name name
       :default default
       :help help
       :namespace *ns*}
      (apply hash-map args)))
    *ns*))


(defn string-parser [flag argument]
  argument)

(defn define-string
  "Registers a flag whose value can be any string."
  [name default help & args]
  (apply define string-parser name default help
         args))


(defn boolean-parser [flag argument]
  (let [argument (string/lower-case argument)]
    (cond
     (some #{argument} ["true" "t" "1"]) true
     (some #{argument} ["false" "f" "0"]) false
     :else
     (throw (Exception. (str "Non-boolean argument to boolean flag: \""
                             argument
                             "\""))))))


(defn define-boolean [name default help & args]
  (apply define boolean-parser name default (or help "a boolean value")
         :boolean true
         args))


(defn integer-parser [flag argument]
  (try
    (Integer/parseInt ^String argument)
    (catch Exception e
      (throw (Exception.
              (str "Could not parse as integer: \"" argument "\""))))))

(defn define-integer [name default help & args]
  "Registers a flag whose value must be an integer."
  (apply define integer-parser name default (or help "an integer value")
         args))


(defn float-parser [flag argument]
  (try
    (Double/parseDouble ^String argument)
    (catch Exception e
      (throw (Exception.
              (str "Could not parse as float: \"" argument "\""))))))

(defn define-float
  "Registers a flag whose value must be a float."
  [name default help & args]
  (apply define float-parser name default (or help "a float value")
         args))


(defn enum-parser [flag argument]
  (if (some #{argument} (:enum-values flag))
    argument
    (throw (Exception. (str "Value should be one of "
                            (string/join "|" (:enum-values flag)))))))

(defn define-enum
  "Registers a flag whose value can be any string from enum_values."
  [name default enum-values help & args]
  (apply define enum-parser name default  (or help "an enum value")
         :enum-values enum-values
         args))


(defn list-parser [flag argument]
  (string/split argument #","))


(defn define-list
  "Registers a glag whose value is a comma-separated list of strings."
  [name default help & args]
  (apply define list-parser name default (or help "a comma-separated list of strings")
         args))


(defn make-multi-parser [parser]
  (fn [flag value-string]
    (let [current-value (:value flag)
          values (if (vector? current-value)
                   current-value
                   (vector current-value))
          new-value (parser flag value-string)]
      (conj values new-value))))


(defn define-multi-string
  "Registers a flag whose value can be a list of any strings.

  Use the flag on the command line multiple times to place multiple
  string values into the list.  The 'default' may be a single string
  (which will be converted into a single-element vector) or a vector of
  strings."
  [name default help & args]
  (apply define (make-multi-parser string-parser)
         name default help
         args))


(defn define-multi-integer
  "Registers a flag whose value can be a list of arbitrary integers.

  Use the flag on the command line multiple times to place multiple
  integer values into the list.  The 'default' may be a single integer
  (which will be converted into a single-element vector) or a vector of
  integers."
 [name default help & args]
  (apply define (make-multi-parser integer-parser)
         name default help
         args))


(defn define-multi-float
  "Registers a flag whose value can be a list of arbitrary floats.

  Use the flag on the command line multiple times to place multiple
  float values into the list.  The 'default' may be a single float
  (which will be converted into a single-element vector) or a vector
  of floats."
  [name default help & args]
  (apply define
         (make-multi-parser float-parser)
         name default help
         args))


(defn help-parser [flag argument]
  (doseq [[_ flag] (:__flags @*flags*)]
    (println (:name @flag) (:help @flag))))


(defn add-default-flags [flag-values]
  (binding [*flags* flag-values]
    (define-string "flagfile" nil
      "Insert flag definitions from the given file into the command line.")
    (define
      help-parser
      "help"
      false
      "Show this help"
      :short-name "?"
      :boolean true)))


(defn make-flag-values []
  (let [flag-values (atom (->FlagValues {} {}))]
    (add-default-flags flag-values)
    flag-values))


(alter-var-root #'*flags* (fn [_] (make-flag-values)))


(defn- full-replacements-for-boolean-flag [flag]
  (let [replacements (reduce into {}
                             (map #(-> [[(str "--" %) (str "--" % "=true")]
                                        [(str "--no" %) (str "--" % "=false")]])
                                  (flag-names flag)))]
    replacements))


(defn cook-boolean-args [flags args]
  (let [boolean-flags (filter :boolean flags)
        replacements (reduce
                      into {} (map full-replacements-for-boolean-flag
                                   boolean-flags))
        cooked-args (map #(get replacements % %) args)]
    cooked-args))


(defn is-flag-file-directive? [^String arg]
  (or (= arg "--flagfile")
      (= arg "-flagfile")
      (.startsWith arg "--flagfile=")
      (.startsWith arg "-flagfile=")))


(defn extract-filename [^String flagfile-str]
  (cond
   (.startsWith flagfile-str "--flagfile=")
   (fs/expand-home (string/trim (subs flagfile-str (count "--flagfile="))))
   (.startsWith flagfile-str "-flagfile=")
   (fs/expand-home (string/trim (subs flagfile-str (count "-flagfile="))))
   :else
   (throw (Exception. "Hit illegal --flagfile type: " (str flagfile-str)))))


(defn get-flag-file-lines [path parsed-file-list]
  (loop [file-lines (line-seq (io/reader path))
         lines []
         parsed-file-list (conj parsed-file-list path)]
    (if-not (seq file-lines)
      [lines parsed-file-list]
      (let [line ^String (string/trim (first file-lines))
            rest-lines (rest file-lines)]
        (cond
         (or (string/blank? line)
             (.startsWith line "#")
             (.startsWith line "//"))
         (recur rest-lines lines parsed-file-list)
         (is-flag-file-directive? line)
         (let [sub-filename (extract-filename line)
               [included-flags parsed-file-list]
               (get-flag-file-lines sub-filename parsed-file-list)]
           (recur rest-lines
                  (concat lines included-flags)
                  parsed-file-list))
         :else
         (recur rest-lines
                (conj lines line) parsed-file-list))))))


(defn read-flags-from-files [args]
  (loop [parsed-file-list []
         rest-of-args args
         new-argv []]
    (if-not (seq rest-of-args)
      new-argv
      (let [current-arg (first rest-of-args)
            rest-of-args (rest rest-of-args)]
        (if (is-flag-file-directive? current-arg)
          (let [[flag-filename rest-of-args]
                (if (or (= current-arg "--flagfile")
                        (= current-arg "-flagfile"))
                  ;; This handles the case of -(-)flagfile foo.  In this
                  ;; case the next arg really is part of this one.
                  (if-not (seq rest-of-args)
                    (throw (Exception. "--flagfile with no argument"))
                    [(fs/expand-home (first rest-of-args)) (rest rest-of-args)])
                  ;; This handles the case of (-)-flagfile=foo
                  [(extract-filename current-arg) rest-of-args])
                [file-argv parsed-file-list]
                (get-flag-file-lines flag-filename parsed-file-list)]
            (recur parsed-file-list
                   rest-of-args
                   (concat new-argv file-argv)))
          (recur parsed-file-list
                 rest-of-args
                 (conj new-argv current-arg)))))))


(defn parse-flags
  "Parses command line flags.  Sets *flags* to be the flag values, and
   returns unparsed arguments.  Note that args should start with
   argv[0]."
  [argv]
  (let [args (read-flags-from-files (rest argv))
        flags (flag-map @*flags*)
        longopts (for [[name _] flags] (str name "="))
        shortopts (string/join
                   (for [[name flag] flags :when (= (count name) 1)]
                     (if (:boolean @flag)
                       name
                       (str name ":"))))
        ;; Correct the argv to support the google style of passing
        ;; boolean parameters.  Boolean parameters may be passed by
        ;; using --mybool, --nomybool, --mybool=(true|false|1|0).
        ;; getopt does not support having options that may or may not
        ;; have a parameter.  We replace instances of the short form
        ;; --mybool and --nomybool with their full forms:
        ;; --mybool=(true|false).
        args (cook-boolean-args (all-flags @*flags*) args)
        [optlist, unparsed-args] (getopt/getopt args shortopts longopts)]
    (swap! *flags* update-flag-values optlist)
    unparsed-args))


