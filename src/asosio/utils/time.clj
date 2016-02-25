(ns asosio.utils.time
  (:require [clj-time.core :as t]
            [clj-time.format :as f]
            [clj-time.coerce :as tcoerce]))

(defn unix-ts-future
  "Return unix timestamp (in seconds) of some future moment.

  Options:
  :days    - number of days from now
  :hours   - number of hours from now
  :minutes - number of minutes from now"
  ([] (unix-ts-future {}))
  ([{:keys [days hours minutes]
     :or {days 0 hours 0 minutes 0}}]
   (let [secs-pr-day 216000
         secs-pr-hour 3600
         secs-pr-minute 60]
     (+ (-> (System/currentTimeMillis)
            (/ 1000) ;; ns=>s
            int)
        (* days secs-pr-day)
        (* hours secs-pr-hour)
        (* minutes secs-pr-minute)))))

;;Painstakingly crafted to match laravel
;;
(def iso8601-no-ms (f/formatter "yyyy'-'MM'-'dd'T'HH':'mm':'ssZZ"))

(defn now-utc
  "Returns current timestamp in ISO 8601 compliant format."
  []
  (t/to-time-zone (t/now) t/utc))

(defn unix-epoch-start
  "Returns the UTC timestamp of January 1st, 1970 - also known as 0 in unix time."
  []
  (t/to-time-zone (t/date-time 1970 01 01) t/utc))

(defn datetime->str
  "Returns datetime as an ISO 8601-formatted string.

  NOTE: mostly an internal helper function, use
        ->dt-str / ->dt-utc-str instead."
  [dt]
  (f/unparse iso8601-no-ms dt))

(defn str->datetime
  "Parses an ISO 8601-formatted string to a datetime.

  NOTE: mostly an internal helper function

  Exceptions:
  * IllegalArgumentException - iff. string cannot be parsed"
  [s]
  (f/parse iso8601-no-ms s))

(defprotocol IToJodaDateTime
  "Protocol that exposes a uniform way to convert user types to
  a Yoda DateTime object."
  (->datetime [this] "Convert user type to Yoda DateTime type."))

(extend-protocol IToJodaDateTime
  org.joda.time.DateTime
  (->datetime [this] (identity this))

  java.sql.Timestamp
  (->datetime [this] (tcoerce/from-sql-time this))

  java.lang.String
  (->datetime [this] (str->datetime this))

  java.util.Date
  (->datetime [this] (tcoerce/from-date this)))

(defprotocol IToJodaDateTimeUTC
  "Protocol that exposes a uniform way to convert user types to
  a Yoda DateTime object using UTC timezone."
  (->utc-datetime [this] "Convert user type to Yoda DateTime type using UTC timezone."))

(extend-protocol IToJodaDateTimeUTC
  org.joda.time.DateTime
  (->utc-datetime [this] (-> (identity this)
                             (t/to-time-zone t/utc)))

  java.sql.Timestamp
  (->utc-datetime [this] (-> (tcoerce/from-sql-time this)
                             (t/to-time-zone t/utc)))

  java.lang.String
  (->utc-datetime [this] (-> (str->datetime this)
                             (t/to-time-zone t/utc)))

  java.util.Date
  (->utc-datetime [this] (-> (tcoerce/from-date this)
                             (t/to-time-zone t/utc))))

(defprotocol IToIso8601NoMsString
  "Protocol that exposes a uniform way to convert user types
  holding a timestamp to an ISO8601 (no ms) formatted string."
  (->dt-str [this] "Convert user datetime type to ISO8601 (no ms) string."))

(extend-protocol IToIso8601NoMsString
  org.joda.time.DateTime
  (->dt-str [this] (-> this
                       (datetime->str)))

  java.sql.Timestamp
  (->dt-str [this] (-> (->datetime this)
                       (datetime->str)))


  java.util.Date
  (->dt-str [this] (-> (->datetime this)
                       (datetime->str))))

(defprotocol IToIso8601NoMsStringUTC
  "Protocol that exposes a uniform way to convert user types
  holding a timestamp to an ISO8601 (no ms) formatted string."
  (->dt-utc-str [this] "Convert user datetime type to ISO8601 (no ms) string using UTC timezone."))

(extend-protocol IToIso8601NoMsStringUTC
  org.joda.time.DateTime
  (->dt-utc-str [this] (-> (->datetime this)
                           (t/to-time-zone t/utc)
                           (datetime->str)))

  java.sql.Timestamp
  (->dt-utc-str [this] (-> (->datetime this)
                           (t/to-time-zone t/utc)
                           (datetime->str)))

  java.util.Date
  (->dt-utc-str [this] (-> (->datetime this)
                           (t/to-time-zone t/utc)
                           (datetime->str))))
