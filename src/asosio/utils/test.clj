(ns asosio.utils.test
  (:require [clj-time.core :as t]))

;; Utility functions for testing code.

;; use this to pull in (& test) otherwise private functions.
(defmacro with-private-fns [[ns fns] & tests]
  "Refers private fns from ns and runs tests in context."
  `(let ~(reduce #(conj %1 %2 `(ns-resolve '~ns '~%2)) [] fns)
     ~@tests))

;; use this instead of (get m k <fallback>) to generate safe fallback values
;; which will provoke test failures.
(defmacro getval
  "ensure a unique test-friendly fallback value if 'k' isn't found in m.

  NOTE: use this instead of (k m) or (get m k <fallback>) to generate fallback
        values which are sure to cause a test failure should either **or both**
        expected & actual result maps miss the entry for key k."
  [m k]
  `(get ~m ~k (str "<" '~m "[" '~k "]" ": no val>")))

(defn dt-within-delta?
  "true iff datetime, act-dt, is equal to the datetime exp-dt within
   some margin of error defined as a delta."
  [exp-dt act-dt {:keys [hrs mins secs] :or {hrs 0 mins 0 secs 0}}]
  (let [delta (list (t/seconds secs) (t/minutes mins) (t/hours hrs))]
    (t/within? (t/interval (apply t/minus exp-dt delta)
                           (apply t/plus  exp-dt delta))
               act-dt)))
