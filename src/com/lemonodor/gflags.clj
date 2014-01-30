(ns com.lemonodor.gflags
  "Gflags for clojure."
  (:require
   [clojure.string :as string]
   [com.lemonodor.getopt :as getopt]))


(defn set-flag-value [flag value-string]
  (assoc flag
    :value ((:parser flag) value-string)
    :present true))


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
        (throw (Exception.
                (str "Flag named \"" (string/join
                                      "\" and \"" (flag-names flag)) "\""
                     " defined in " (:namespace flag)
                     " conflicts with flag named \""
                     (string/join
                      "\" and \"" (flag-names @conflicting-flag)) "\""
                     " defined in " (:namespace @conflicting-flag)))))
      (->FlagValues (merge (:__flags this) name-entries)
                    (update-in flags-by-ns [ns] conj flag-atom))))
  (update-flag-values [this optlist]
    (doseq [[optname value-string] optlist]
      (let [name (optname-to-flag-name optname)]
        (swap! ((:__flags this) name) set-flag-value value-string)))
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
(defn flags []
  (into
   {}
   (for [[name flag] (:__flags @*flags*)]
     [(flag-name-to-symbol name) (:value @flag)])))


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
     serializer
     allow-override])


(defn string-parser [argument]
  argument)

(defn string-serializer [value]
  value)


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


(defn define-string [name default help & args]
  (apply define string-parser name default help :serializer string-serializer args))


(defn boolean-parser [argument]
  (let [argument (string/lower-case argument)]
    (cond
     (some #{argument} ["true" "t" "1"]) true
     (some #{argument} ["false" "f" "0"]) false
     :else
     (throw (Exception. (str "Non boolean argument to boolean flag: " argument))))))


(defn define-boolean [name default help & args]
  (apply define boolean-parser name default (or help "a boolean value")
         :boolean true
         args))


(defn add-default-flags [flag-values]
  (binding [*flags* flag-values]
    (define-string "flagfile" nil
      "Insert flag definitions from the given file into the command line.")))


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


(defn parse-flags
  "Parses command line flags.  Sets *flags* to be the flag values, and
   returns unparsed arguments.  Note that args should start with
   argv[0]."
  [argv]
  (let [args (rest argv)
        flags (flag-map @*flags*)
        longopts (for [[name _] flags] (str name "="))
        shortopts (string/join
                   (for [[name flag] flags :when (= (count name) 1)]
                     (if (:boolean flag)
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
