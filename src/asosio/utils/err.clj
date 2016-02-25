(ns asosio.utils.err
  (:require [clojure.algo.monads :refer [domonad defmonad maybe-m]]
            [taoensso.timbre :as timbre]
            [asosio.utils.rsp :as rsp]))

(defn pp-stacktrace
  "get a string representation of the throwable's
  stacktrace."
  [t]
  (-> "\n  "
      (str (->> (.getStackTrace t)
                (interpose "\n  ")
                (apply str)))
      (str "\n---\n")))

;; Error-handling
(defrecord Failure
    [cause ctx])

;; Same as 'Failure' - except we expect
(defrecord RestFailure
    [cause ctx rsp])

(defprotocol ComputationFailed
  "A protocol to determine if a computation has resulted in a failure.
  Allows distinction of failures and ret-vals w/o restricting set of possible
  values. Allows error-m's error-recognition logic to be extended with
  new types if need be."
  (has-failed? [self]))

(extend-protocol ComputationFailed
  nil
  (has-failed? [self] false)

  Object
  (has-failed? [self] false)

  Failure
  (has-failed? [self] true)

  RestFailure
  (has-failed? [self] true)

  Exception
  (has-failed? [self] true))

(defprotocol FailureObject
  "A protocol to govern how to extract the error value of a
  failed computation."
  (fail-cause [obj])
  (fail-ctx [obj]))

(extend-protocol FailureObject
  Failure
  (fail-cause [self]
    (:cause self))
  (fail-ctx [self]
    (:ctx self))

  RestFailure
  (fail-cause [self]
    (or (:cause self) (:msg self)))
  (fail-ctx [self]
    (or (let [ctx (:ctx self)]
          (if (instance? Exception ctx)
            (str (fail-cause ctx) "\n---\n" (fail-ctx ctx))
            (:ctx self)))
        (:rsp self)))

  Exception
  (fail-cause [self]
    (.getMessage self))
  (fail-ctx [self]
    (pp-stacktrace self)))

(defprotocol RestFailureObject
  "Governs how to extract a REST response from a RestFailure
  object returned as the result of a failed computation."
  (has-fail-rsp? [obj])
  (fail-rsp [obj]))

(extend-protocol RestFailureObject
  Object ;; (almost) catch-all
  (has-fail-rsp? [self] false)
  (fail-rsp [self] nil)

  RestFailure
  (has-fail-rsp? [self]
    (if (:rsp self) true false))
  (fail-rsp [self]
    (:rsp self)))

(defmonad error-m
  "A failure-aware variant of the maybe-m monad. If
  any monad yields a failure, further processing is
  aborted and the failure is returned. Otherwise the
  value of the final monad is returned."
  [m-result identity
   m-bind (fn [m f] (if (has-failed? m)
                      m
                      (f m)))])

(defmacro attempt-all
  "attempt-all attempts evaluating each form in bindings, binding
  the result to the specified variable. If an evaluated form yields
  a failure (ComputationFailed protocol) further evaluation is aborted
  and the failure is either returned or passed to an optional
  error-handler."
 ([bindings return] `(domonad error-m ~bindings ~return))
 ([bindings return on-err-fn]
  `(let [result# (attempt-all ~bindings ~return)]
     (if (has-failed? result#)
       (apply ~on-err-fn [result#])
       result#))))

(defmacro any
  "Evaluates exprs one at a time, from left to right. If a form
  returns a non-failure value, (= (has-failed? form) false),
  then 'any' returns that value, skipping the remaining expressions.
  Otherwise, the value of the last expression is returned. 'any'
  returns nil if no exprs are supplied."
  ([] nil)
  ([x] x)
  ([x & next]
   `(let [any# ~x]
      (if (has-failed? any#) (any ~@next) any#))))

(defmacro exc-capt
  "Capture (checked) exceptions and return them as regular values.
  Primarily of interest to 'attempt-all' and similar functions
  making use of the ComputationFailed protocol."
  [& body]
  `(try ~@body
        (catch Exception e#
          e#)))

(defn handle-failure
  "handle-failure will ensure a REST response message is sent
  indicating the error (and optionally, log the error).

  Example (using a RestFailure wrapping an exception)
  (err/handle-failure
    (err/map->RestFailure
      {:cause \"ERR! deleting a pin record failed!\"
       :ctx exc
       :rsp (rsp/err {:msg \"failed to delete pin\"})
       :log-lvl :error}))"
  ([failure] (handle-failure failure {}))
  ([failure
    {:keys [log log-lvl rsp-status rsp-headers rsp-msg rsp-apicode]
     :or {log true log-lvl :error}}]
   (timbre/debug "LOVE YOU FOREVER")
   (timbre/debug failure)
   (do
     ;; Log error (if appliccable)
     (if log
       (timbre/log log-lvl
                   "\n"
                   "Failure: " (fail-cause failure)
                   "\n"
                   "Details: " (fail-ctx failure)))
     ;; Return REST response
     (if (has-fail-rsp? failure)
       (fail-rsp failure)
       (->> {:status rsp-status
             :headers rsp-headers
             :msg (or rsp-msg "server encountered an internal error")
             :apicode rsp-apicode}
            (filter (comp not nil? val)) ;; val is apparently magic.
            (into {})
            (rsp/err))))))
