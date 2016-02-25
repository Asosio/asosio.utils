(ns asosio.utils.middleware
  (:require [ring.util.response]
            [taoensso.timbre :as timbre]
            [asosio.utils.time :as at]
            [asosio.utils.rsp :as rsp])
  (:import com.mysql.jdbc.exceptions.jdbc4.CommunicationsException
           com.mchange.v2.resourcepool.CannotAcquireResourceException))

(defn wrap-x-fetched-at
  "Add 'X-Fetched-At' which details the time at which
  the request was handled (server time)."
  [handler]
  (fn [request]
    (if-let [response (handler request)]
      (ring.util.response/header response "X-Fetched-At"
                  (at/->dt-utc-str (at/now-utc))))))

(defn wrap-keywordize-qparams
  "Keywordize query parameters."
  [handler]
  (fn [rq]
    (->> (update rq :query-params (fn [m] (clojure.walk/keywordize-keys m)))
         (merge rq)
         (handler))))

(defn wrap-catch-exceptions
  "Handle exceptions which may arise during request processing."
  [handler]
  (fn [rq]
    (let [gw-timeout
          (fn [exc msg]
            (timbre/debug (Exception. msg exc))
            (rsp/err {:status rsp/HTTP_GATEWAY_TIMEOUT
                      :msg msg}))
          internal-err
          (fn [exc msg]
            (timbre/debug (Exception. msg exc))
            (rsp/err {:status rsp/HTTP_INTERNAL_SERVER_ERROR
                      :msg msg}))]
      (try
        (handler rq)
        (catch CommunicationsException e
          (gw-timeout e "database connection timed out"))
        (catch java.sql.SQLException e
          (let [cause (.getCause e)]
            (if (instance? CannotAcquireResourceException cause)
              (gw-timeout cause "failed to connect to database")
              (internal-err e "database request provoked an internal server error"))))
        (catch Exception e
          (internal-err e "Uncaught/unhandled internal error"))))))
