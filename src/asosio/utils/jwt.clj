(ns asosio.utils.jwt
  (:require [clj-jwt.core :as jwtc]
            [clj-jwt.key :as jwtk]
            [clj-time.core :as tc]
            [environ.core :refer [env]]
            [asosio.utils.err :as err]
            [asosio.utils.rsp :as rsp]
            [asosio.utils.time :as at]))

(defn- jwt-algorithm*
  "Get the JWT token algorithm to use."
  []
  (-> (env :jwt-algorithm) (clojure.string/upper-case) keyword))

(def
  ^{:doc
    "Get the JWT token algorithm to use."}
  jwt-algorithm
  (memoize jwt-algorithm*))

(defn tkn
  "Parse the received JWT token header.

  NOTE: will return a RestFailure if things fails."
  [tkn-str]
  (try
    (jwtc/str->jwt tkn-str)
    ;; technically
    ;; "" => Exception "JSON error"
    ;; "...." (where leading 'ey' is missing) => java.io.IOException
    (catch Exception e
      (err/map->RestFailure
       {:msg "failed to parse JWT token"
        :rsp (rsp/err {:msg "Invalid token (invalid format)."})
        :log-lvl :debug}))))

(defn tkn->claim
  "Extract value of given claim from the token

  NOTE: unless optional is set to true, missing claims will
        yield a RestError value."
  ([tkn claim] (tkn->claim tkn claim {}))
  ([tkn claim {:keys [optional] :or {optional false}}]
   (if-let [v (get-in tkn [:claims claim] nil)]
     v
     (if optional
       nil
       (err/map->RestFailure
        {:msg ""
         :rsp (rsp/err {:msg (str "Invalid token (missing claim '" (name claim) "')")})
         :log-lvl :debug})))))

(defn sig-verify
  "Verify the token signature, ensuring the issued token is
  signed with the same secret & algorithm we use."
  ([tkn] (sig-verify tkn {}))
  ([tkn
    {:keys [secret algo]
     :or {secret (env :jwt-secret) algo (jwt-algorithm)} :as opts}]
   (if-let [_ (jwtc/verify tkn (jwt-algorithm) (env :jwt-secret))]
     ;; tkn signature checked out
     (if-let [exp (tkn->claim tkn :exp {:optional true})]
       ;; tkn can expire
       (if (>= exp (at/unix-ts-future))
         ;; tkn signature not expired yet
         true
         ;; tkn signature expired
         (err/map->RestFailure
          {:msg "token expired"
           :rsp (rsp/err {:msg "Invalid token (expired)"})
           :log-lvl :debug}))
       ;; tkn cannot expire (hence valid)
       true)
     ;; tkn signature did not check out
     (err/map->RestFailure
      {:msg "failed to verify token signature"
       :rsp (rsp/err {:msg "Invalid token (verification failed)."})
       :log-lvl :debug}))))

(defn tkn->claim-and-verify
  ([tkn claim verification-fn]
   (tkn->claim-and-verify tkn claim verification-fn {}))
  ([tkn claim verification-fn {:keys [optional] :or {optional false} :as opts}]
   (let [claim-val (tkn->claim tkn claim opts)]
     (cond (or (and (nil? claim-val) optional)
               (and ((complement nil?) claim-val)
                    (verification-fn claim claim-val)))
           claim-val
           true ; default
           (let [err (str "Invalid token (claim '" (name claim) "' invalid)")]
             (err/map->RestFailure
              {:msg err
               :rsp (rsp/err {:msg err})
               :log-lvl :debug}))))))

(defn sign
  "Create a signed & hashed JWT token from a map of claims."
  ([claims] (sign claims {}))
  ([claims
    {:keys [secret algo]
     :or {secret (env :jwt-secret) algo (jwt-algorithm)} :as opts}]
   (-> claims (jwtc/jwt) (jwtc/sign algo secret) (jwtc/to-str))))
