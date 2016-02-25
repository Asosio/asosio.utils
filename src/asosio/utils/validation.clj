(ns asosio.utils.validation
  (:require [validateur.validation :as v]
            [asosio.utils.rsp :as rsp]
            [asosio.utils.helpers :as ah]
            [asosio.utils.err :refer [map->RestFailure]]
            [asosio.utils.time :as at]))

(defn pred-utc-datetime
  [attr val]
  (try
    (do
      (at/str->datetime val)
      true)
    (catch IllegalArgumentException e
      false)))
(def
  ^{:const true
    :doc
    "Message to return for malformed dates
    (use in conjunction with above predicate fn)"}
  pred-utc-datetime-msg
  "expected format: 'YYYY-MM-DDThh:mm:ss+00:TZ' where TZ is the offset from GMT")

(defn pred-string
  [attr val]
  (string? val))

(defn pred-email
  [attr val]
  (ah/email? val))

(defn pred-color-hex
  [attr val]
  (re-find (re-pattern "^#[0-9a-fA-F]{6}$") val))

(defn validate-with-predicate
  "Returns a function that, when given a map, validates that the value
  referred to by the attribute satisfies the supplied predicate function.

  Arguments:
  attribute: key referring to the argument to validate
  predicate-fn: (fn [attribute val]) -> truthy/falsy

  Accepted options:
  :message (default:\"must satisfy requirements\"): returned error message
  :blank-message (default:\"cannot be blank\"): returned iff value isn't present
  :message-fn: function with which to derive the error message
               signature: (fn [type map attribute & args]).
               Type will be :blank/:acceptance.
  :allow-nil? (default: false): should nil values be allowed?"
  [attribute predicate-fn
   & {:keys [allow-nil? message blank-message message-fn]
      :or {allow-nil? false,
           message "must satisfy requirements"
           blank-message "cannot be blank"}}]
  (let [get-fn (if (vector? attribute) get-in get)
        msg-fn (fn [t m msg & args]
                 (if message-fn (apply message-fn t m attribute args) msg))]
    (fn [m]
      (if-let [val (get-fn m attribute)]
        (if (predicate-fn attribute val)
          [true {}]
          [false {attribute #{(msg-fn :acceptance m message)}}])
        (if allow-nil?
          [true {}]
          [false {attribute #{(msg-fn :blank m blank-message)}}])))))

(defn- f-validation-errs
  "Transform validateur error maps into those used across our services."
  [vres]
  {:error
   {:code rsp/RQ_VALIDATION_ERR,
   :message "Data does not meet requirements."
   :details
   (reduce (fn [acc [k v]]
             (cons {:field (name k) :errors v} acc))
             '()
             vres)}})

(defn input-valid?
  [vset data]
  (let [vres (vset data)]
    (if-not (v/valid? vres)
      (map->RestFailure
       {:msg "validation errors encountered"
        :rsp (rsp/base {:status rsp/HTTP_UNPROCESSABLE_ENTITY
                        :body (f-validation-errs vres)})
        :log-lvl :debug}))))
