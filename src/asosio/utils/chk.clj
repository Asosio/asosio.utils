(ns asosio.utils.chk
  (:require [clojure.string :as cs]
            [asosio.utils.rsp :as rsp]
            [asosio.utils.helpers :as ah]
            [asosio.utils.err :refer [map->RestFailure]]
            [asosio.utils.jwt :as jwt]
            [asosio.utils.err :as err]))

(defn mediatype-in?
  [mimetype acceptable]
  (if-not
      (ah/in? (-> (cs/split mimetype #";")
                  (first))
              acceptable)
    (map->RestFailure
     {:msg "rq content-type was unacceptable"
      :rsp (rsp/err {:status rsp/HTTP_BAD_REQUEST
                     :msg {:message "Wrong content type"
                           :apicode rsp/RQ_INVALID_INPUT
                           :details {:received mimetype :accepts acceptable}}})
      :log-lvl :debug})))

(defn tkn-claim-eq?
  ([tkn claim expected] (tkn-claim-eq? tkn claim expected {} {}))
  ([tkn claim expected opts] (tkn-claim-eq? claim expected opts {}))
  ([tkn claim expected
    ;; opts
    {:keys [msg-fn]
     :or {msg-fn
          (fn [claim exp actual] (str "Invalid token (expected claim '" claim
                                      "' to be '" exp "', but got: '" actual "')"))}
     :as opts}
    ;; claim-opts
    {:keys [optional] :or {optional false} :as claim-opts}]
   (let [v (jwt/tkn->claim tkn claim claim-opts)
         ret-err (fn [actual]
                   (let [msg (msg-fn claim expected actual)]
                     (err/map->RestFailure
                      {:msg (str "tkn-claim-eq? err: " msg)
                       :rsp (rsp/err
                             {:msg msg
                              :status rsp/HTTP_BAD_REQUEST
                              :apicode rsp/RQ_INVALID_INPUT})
                       :log-lvl :debug})))]
     (cond
       (err/has-failed? v) ;; no value returned
       (ret-err nil)
       ((complement =) v expected) ;; wrong value
       (ret-err v)
       true ;; default, all good
       true))))
