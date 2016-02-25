(ns asosio.utils.helpers
  (:require [clojure.string :as cs])
  (:import java.io.FileNotFoundException))

(defn matches-regex?
  "Returns true if the string matches the given regular expression"
  [v regex]
  (boolean (re-matches regex v)))

(defn email?
  "Returns true if v is an email address"
  [v]
  (if (nil? v)
    false
    (matches-regex? v #"(?i)[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?")))

(defn capitalize
  "Capitalize the first letter and every
  letter preceded by the specified separator."
  [key & {:keys [sep] :or {sep #" "}}]
  (cs/join
   sep
   (map cs/capitalize (cs/split key sep))))

(defn map-pair
  "Yield the result of applying f to each element of the
  map, passing both key and value along to f which should
  return a map containing the new entry."
  [m f]
  (apply merge (map (fn [[k v]] (f k v)) m)))

(defn map-vals
  "Yield the result of applying f to each value of the map."
  [m f]
  (apply merge (map (fn [[k v]] {k (f v)}) m)))

(defn map-has-keys?
  "Returns true iff all keys in list/vector ks are present in the map m."
  [m ks]
  {:pre [(map? m) (or (vector? ks) (list? ks))]}
  (every? #(contains? m %) ks))

(defn mapply
  "Apply contents of map 'args' to function 'f'
  as named parameters."
  [f & args]
  (apply f (apply concat (butlast args) (last args))))

(defmacro when-require [n & body]
  (let [nn (eval n)]
    (try (require nn)
      (catch Throwable e nil))
    (when (find-ns nn)
      `(do ~@body))))

(defmacro if-require
  "Requires the specified libspec executing the 'then' form
  if successful. Otherwise, the 'else?' form if supplied."
  [ns then & else? ]
  `(do
     (let [ns# ~ns]
       (try
         (require ns#)
         (catch Throwable ~'e nil))
       (if
         (find-ns (cond (symbol? ns#) ns# (vector? ns#) (first ns#)))
         (do ~then)
         (do ~@else?)))))

(defn str->num
  "Convert a string to a number - providing a sane
  default if conversion fails."
  ([s] (str->num s 0))
  ([s n]
   (try (Integer. s)
        (catch NumberFormatException e n))))

(defn ->num
  "Try to convert an incoming value to a number.
  Returns a fallback value iff conversion fails AND one is defined.
  Yields an exception iff conversion fails AND no fallback value
  has been defined."
  ([input] (->num input nil))
  ([input fallback]
  (cond
    (string? input) (str->num input)
    (number? input) input
    ((complement nil?) fallback) fallback
    :else (throw (Exception. (str "Invalid number supplied: " input))))))

(defn in?
  "true iff seq contains elm."
  [elm seq]
  (some #(= elm %) seq))

(defn str-trunc
  "Truncate string iff greater than some size."
  [s mlen]
  (subs s 0 (min (count s) mlen)))
