(ns com.lemonodor.gflags
  "Gflags for clojure."
  (:require
   [clojure.string :as string]
   [com.lemonodor.getopt :as getopt]))


(defprotocol FlagValuesProtocol
  (register-flag [this flag ns]))


(defrecord FlagValues [__flags __flags_by_ns]
  FlagValuesProtocol
  (register-flag [this flag ns]
    (let [flags-by-ns (:__flags_by_ns this)]
      (->FlagValues (assoc (:__flags this) (:name flag) flag)
                    (assoc flags-by-ns ns (conj (flags-by-ns ns) flag))))))


(def ^:dynamic *flags* (atom (->FlagValues {} {})))


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


(defn define-flag [flag-values flag ns]
  (swap! flag-values register-flag flag ns))

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


(defn parse-flags [& whatever]
  nil)
