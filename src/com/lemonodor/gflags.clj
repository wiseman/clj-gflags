(ns com.lemonodor.gflags
  "Gflags for clojure."
  (:require
   [clojure.string :as string]
   [com.lemonodor.getopt :as getopt]))


(defprotocol FlagValuesProtocol
  (register-flag [this flag ns])
  (flag-map [this]))


(defrecord FlagValues [__flags __flags_by_ns]
  FlagValuesProtocol
  (register-flag [this flag ns]
    (let [flags-by-ns (:__flags_by_ns this)
          name-entries (if (:short-name flag)
                         {(:short-name flag) flag
                          (:name flag) flag}
                         {(:name flag) flag})]
      (->FlagValues (merge (:__flags this) name-entries)
                    (assoc flags-by-ns ns (conj (flags-by-ns ns) flag)))))
  (flag-map [this]
    (:__flags this)))


(def ^:dynamic *flags* (atom (->FlagValues {} {})))


(defn flag-values
  ([values] (into {}
                  (for [[name flag] (:__flags @values)] [name flag])))
  ([] (flag-values *flags*)))


;; Equivalent of gflags.py FLAGS.  FIXME: Should be able to return a
;; thing that acts like a map but just indexes into existing flag
;; values without actually creating a map, right?
(defn flags
  ([values] (into {}
                  (for [[name flag] (:__flags @values)] [name (:value flag)])))
  ([] (flags *flags*)))

(defrecord Flag
    [name
     default
     value
     help
     short-name
     boolean
     present
     parser
     serializer
     allow-override])


(defn string-parser [argument]
  argument)

(defn string-serializer [value]
  value)


(defn define-flag [flagvalues flag ns]
  (swap! flagvalues register-flag flag ns))

(defn define [parser name default help serializer & args]
  (define-flag
    *flags*
    (map->Flag
     (merge
      {:help "(no help available)"}
      {:parser parser
       :name name
       :default default
       :help help
       :serializer serializer}
      (apply hash-map args)))
    nil))


(defn define-string [name default help & args]
  (apply define string-parser name default help string-serializer args))


(defn boolean-parser [argument]
  (let [argument (string/lower-case argument)]
    (cond
     (some #{argument} ["true" "t" "1"]) true
     (some #{argument} ["false" "f" "0"]) false
     :else
     (throw (Exception. (str "Non boolean argument to boolean flag: " argument))))))

(defn define-boolean [name default help & args]
  (apply define boolean-parser name default
         (or help "a boolean value")
         nil
         args))


(defn parse-flags
  ([argv] (parse-flags argv *flags*))
  ([argv flagvalues]
     (let [args (rest argv)
           flags (flag-map @flagvalues)
           longopts (for [[name _] flags] (str name "="))
           shortopts (apply str
                            (for [[name flag] flags :when (= (count name) 1)]
                              (if (:boolean flag)
                                name
                                (str name ":"))))
           x (do (prn "longopts for getopt" longopts)
                 (prn "shortopts for getopt" shortopts)
                 (prn "args for getopt" args))
           [optlist, unparsed-args] (getopt/getopt args shortopts longopts)]
       (prn "optlist returned by getopt" optlist)
       (prn "unparsed args returned by getopt" unparsed-args)
       unparsed-args)))
