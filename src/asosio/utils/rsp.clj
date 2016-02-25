(ns asosio.utils.rsp
  (:require [asosio.utils.time :as at]))

(def ^:const HTTP_OK 200)
(def ^{:const true
      :doc
      "Sent in response to successful POST requests.
      Should be accompanied a HTTP header, 'LOCATION'
      which contains the URI to the new object."}
  HTTP_CREATED 201)
(def ^{:const true
      :doc
      "Request fulfilled but no new data to return to client.
      The client is obligated not to update its view in response"}
  HTTP_NO_CONTENT 204)

(def ^{:const true
      :doc
      "Signal to client that the resource has permanently
      moved.

      Should include the new location in the
      response as a \"Location\" header. Client browsers
      will automatically follow the redirect and remember
      it."}
  HTTP_PERMANENTLY_MOVED 301)

(def ^{:const true
      :doc
      "Request couldn't be understood by server due to
      malformed syntax. The client is obligated NOT to
      repeat its request without first modifying it."}
  HTTP_BAD_REQUEST 400)

(def ^{:const true
      :doc
      "Request is understood but the service refuses to
      fulfill it. Authorization will not help and the
      client is obligated not to repeat it.

      NOTE: should only be used when the server wishes
            to explain why the request is refused. If
            the server does not wish so, use 404
            instead."}
  HTTP_FORBIDDEN 403)

(def ^{:const true
       :doc
       "No resource matches the request URI. No indication
       of whether or not this is permanent (if so, use
       410 [GONE]).

       Also commonly used when the server doesn't wish to
       reveal exactly why a request was refused."}
  HTTP_NOT_FOUND 404)

(def ^{:const true
       :doc
       "Request could not be completed due to a conflict with
       the current state of the resource.

       Response SHOULD provide sufficient detail for the
       client to understand what is wrong and how to resolve
       it."}
  HTTP_CONFLICT 409)

(def ^{:const true
       :doc
       "The request's content-type isn't supported for the
       this URI."}
  HTTP_UNSUPPORTED_MEDIA_TYPE 415)

(def ^{:const true
       :doc
       "Server understands the content-type of the request
       entity (thus not 415 [unsupported media]).
       Request entity is also syntactically correct (thus
       not 400 [Bad request]).

       The server is, however, unable to process the
       request. Typical if the request was well-formed but
       semantically erroneous."}
  HTTP_UNPROCESSABLE_ENTITY 422)
; Uncaught exceptions tumbling to the very top
(def ^{:const true
      :doc
      "The server encountered an unexpected condition
      which prevented it from fulfilling the request.
      (We intend these for top-level exception handling.)"}
  HTTP_INTERNAL_SERVER_ERROR 500)

(def ^{:const true
      :doc
      "The server (acting as a proxy) did not receive
      a timely response from the upstream server specified by
      the URI/some auxiliary server needed to complete the request."}
  HTTP_GATEWAY_TIMEOUT 504)

(def ^{:const true
      :doc
      "Request is successful"}
  RQ_SUCCESSFUL 400)

(def ^{:const true
      :doc
      "While the token is valid, the request exceeds
      the privileges of that user."}
  RQ_UNAUTHORIZED 1)

(def ^{:const true
      :doc
      "The token is either fabricated, since expired
      or of the wrong type (e.g. gTkn/cTkn)."}
  RQ_INVALID_TOKEN 2)

(def ^{:const true
      :doc
      "Input is invalid (e.g. expected JSON, expected JPEG-file etc)"}
  RQ_INVALID_INPUT 3)

(def ^{:const true
      :doc
      "Missing parameters"}
  RQ_MISSING_PARAMS 4)

(def ^{:const true
      :doc
      "One or more parameters are malformed"}
  RQ_VALIDATION_ERR 5)

(def ^{:const true
      :doc
      "Request triggered an internal server error"}
  RQ_SRV_ERR 6)

(def ^{:const true
      :doc
      "Request is successful"}
  RQ_INVALID 7)

(def index-limit
  "Maximum number of results returned by one index request."
  10)

(defn paginate-by-time
  "Return a response paginated by a time column."
  [{:keys [data next has-more? deleted] :as args}]
  {:data data
   :pagination {:next (at/->dt-utc-str next)
                :hasMore has-more?}})

(defn base
  [{:keys [status headers body] :or {status HTTP_OK headers {}}}]
  {:status status
   :headers {}
   :body body})

(defn err
  [{:keys [status headers msg apicode]
    :or {status HTTP_INTERNAL_SERVER_ERROR
         headers {} apicode RQ_SRV_ERR}}]
  (base {:status status
         :headers headers
         :body {:error {:code apicode
                        :message msg}}}))

(defn ok
  "Skeletal ring response - HTTP_OK(200) and no headers."
  [body & {:keys [headers] :or {headers {}}}]
  {:status 200
   :headers headers
   :body body})

(defn not-found
  []
  {:status HTTP_NOT_FOUND
   :headers {}
   :body nil})

(defn created
  "Typical response to a successful POST request."
  ([] (created {}))
  ([{:keys [url key-name key-val body] :or {:body {}}}]
   (->> {:status HTTP_CREATED
         :body
         (if (some nil? [key-name key-val])
           body
           (merge body {:key key-name :id key-val}))}
        (merge (if (nil? url) {} {:headers {"Location" url}})))))

(defn destroyed
  "Typical response to a DELETE request."
  []
  {:status HTTP_NO_CONTENT
   :headers {}
   :body nil})

(defn updated
  "Typical response to a PUT request."
  []
  {:status HTTP_NO_CONTENT
   :headers {}
   :body nil})

(defn moved-to
  "Instruct clients to fetch resource elsewhere."
  [url]
  {:status HTTP_PERMANENTLY_MOVED
   :headers {"Location" url}
   :body nil})

(defn handle
  [ chk rsp-fnc httpcode & xs ]
  (if (-> (count xs) (mod 3) (zero?) (not))
    (throw (IllegalArgumentException. "Input must come in groups of 3 (<chk> <rsp fnc> <httpcode>)"))
    (loop [chk chk rsp-fnc rsp-fnc httpcode httpcode xs xs]
      (if (nil? (and chk rsp-fnc httpcode))
        nil
        (let [chk-res (chk)]
          (if-not (nil? chk-res)
            (rsp-fnc chk-res httpcode)
            (let [[chk rsp-fnc httpcode & xs] xs]
              (recur chk rsp-fnc httpcode xs))))))))

(defmacro with-exc->rsp
  "Wraps the form in an exception handler which, if triggered
  will return a HTTP/500 (internal server error) to the client."
  [form]
  `(try
   ~form
   (catch Exception ~'e
     (err {:msg "internal server error"} HTTP_INTERNAL_SERVER_ERROR))))

(defn api-fk-key
  "given a resource type (\"post\", \"wall\", \"application\")
  the corresponding term used in the API to describe the
  resource's id is returned (e.g. :pid, :wid, :aid)."
  [resource-type]
  (try
    (case resource-type
      "post"
      :pid
      "event"
      :eid
      "file"
      :fid
      "wall"
      :wid
      "application"
      :aid)
    (catch IllegalArgumentException e
          :id))) ;; fall back to :id
