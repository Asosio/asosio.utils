(ns asosio.utils.contracts)

(defn- failure?
  "True iff the value is a failure-type object."
  [v]
  (or (instance? asosio_utils.err.Failure v)
      (instance? asosio_utils.err.RestFailure v)))

(defn nil-or-failure?
  "Contract enforcing a nil value (or a failure)."
  [v]
  (or (nil? v)
      (failure? v)))

(defn map-or-failure?
  "Contract enforcing a map value (response map) or a failure."
  [v]
  (or (map? v) (failure? v)))

(defn opt-arg?
  "Contract for an optional argument."
  [pred arg]
  (or (nil? arg) (pred arg) false))
