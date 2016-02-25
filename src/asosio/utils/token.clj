(ns asosio.utils.token
  (:require [asosio.utils.jwt :as jwt]))

(defn verify-cid?
  [tkn cid]
  (jwt/tkn->claim-and-verify
   tkn :cid
   (fn [claim claim-val] (= claim-val cid))))

(defn verify-uid?
  [tkn uid]
  (jwt/tkn->claim-and-verify
   tkn :uid
   (fn [claim claim-val] (= claim-val uid))))

(defn verify-claim-aud
  [tkn aud val]
  (jwt/tkn->claim-and-verify
   tkn :aud
   (fn [claim claim-val] (= claim-val val))))
