(ns asosio.utils.dbtypes
  (:require [jdbc.proto :as proto]
            [clj-time.coerce :as tc]))

(extend-protocol proto/ISQLType
  org.joda.time.DateTime

  ;;Convert user type to SQL Type
  (as-sql-type [self conn]
    (tc/to-timestamp self))

  ;; Assign usertype to the argument in the prepared
  ;; statement indicated by 'index'
  (set-stmt-parameter! [this conn stmt index]
    (.setTimestamp stmt index (proto/as-sql-type this conn))))
