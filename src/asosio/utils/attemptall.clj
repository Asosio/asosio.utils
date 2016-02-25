(ns asosio.utils.attemptall
  (:require [asosio.utils.err :as err]
            [asosio.utils.rsp :as rsp]
            [asosio.utils.jwt :as jwt]))

(defn rq->uid*
  [rq]
  (if-let [tkn-str (get-in rq [:headers :token])]
    (err/attempt-all
     [tkn (jwt/tkn tkn-str)
      uid (jwt/tkn->claim tkn :uid)]
     uid)
    (err/map->RestFailure
     {:msg "Failed to parse JWT Token: no token supplied"
      :rsp (rsp/err {:msg "Invalid Token (no token)."})
      :log-lvl :debug})))
