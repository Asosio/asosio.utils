(ns asosio.utils.timequery
  (:require [honeysql.helpers :as hh]
            [taoensso.timbre :as timbre]
            [clj-time.core :as t]
            [asosio.utils.helpers :as ah]
            [asosio.utils.time :as at]))

;; TODO
;; ----------------
;; Despite the rewrite, this is still a bit messy.
;;
;; In particular, the tests against rsp-map are nigh useless (and don't reflect
;; actual usage).
;;
;; Regarding after/before
;; -----------------------
;; after/before/since are inclusive meaning the value specified is actually included.
;; In case I forget again, here's the case for why that is:
;; * When fetching backwards in time (refresh/before query) we'll either be at the end
;;   (at which point the 'next' value is moot) or we'll fetch a subset of the remaining
;;   entries. In the latter case, the 'next' value will actually be the exact value of
;;   the first item of the next batch (hence inclusive matching is nice)
;;
;; * When fetching new items since added (after-query) it's not ideal.
;;   HOWEVER - we really cannot know if something came in after - however improbable,
;;   someone might manage to get in a post at the same time as the newest post we've
;;   got (unlikely, but seemingly possible?) - hence fetching from the exact same TS
;;   and up should guarantee that no posts fall through the gaps.

(def ^{:const true
       :doc "(Default) maximum number of entries fetched per index call"}
  INDEX_LIMIT_DEFAULT 10)

(def ^{:const true
       :doc "(Default) direction of time-queries (i.e. => back in time)"}
  QCTX_DIR_DEFAULT :older)

(defn extract-rq-time-params
  "Extracts timequery-related request parameters from request map."
  [query-params]
  (merge
   ;; Get time-related query params (and if present) covert to datetime objects
   (-> (select-keys query-params [:since :before :after])
       (ah/map-pair (fn [k v] (if v {(keyword k) (at/->datetime v)}))))
   ;; get 'limit' request param (& convert key to keyword)
   {:limit (ah/str->num (get query-params :limit) INDEX_LIMIT_DEFAULT)}))

(defn- qwrap-refresh-query
  [qmap created-col updated-col limit]
  (-> (hh/merge-where qmap [:and
                            [:<= created-col :?before]
                            [:>= created-col :?after]
                            [:>  updated-col :?since]])
      (hh/order-by [[updated-col :asc]])
      (hh/limit limit)))

(defn- refresh-query
  [qmap dqmap
   {:keys [after before since] :as rq-params}
   {:keys [index-limit created-col updated-col deleted-col] :as opts}]
  {:pre [(map? qmap) (map? rq-params) (map? dqmap)
         (ah/map-has-keys? rq-params [:after :before :since])
         (ah/map-has-keys? opts [:index-limit :created-col :updated-col :deleted-col])]

   :post [(map? %) (ah/map-has-keys? % [:limit :created-col :updated-col :deleted-col
                                        :sort-by-key :dir
                                        :query :query-args :del-query :del-query-args])]}
  (let [limit (min index-limit (get rq-params :limit index-limit))]
    (merge
     (select-keys opts [:created-col :updated-col :deleted-col])
     {:dir :older :sort-by-key updated-col :limit limit
      :query (qwrap-refresh-query qmap created-col updated-col (inc limit))
      :query-args {:after after :before before :since since}
      :del-query (hh/merge-where dqmap [:>= deleted-col :?since])
      :del-query-args {:since since}})))

(defn- qwrap-after-query
  [qmap created-col limit]
  (-> (hh/merge-where qmap [:>= created-col :?after])
      (hh/order-by [[created-col :asc]])
      (hh/limit limit)))

(defn- after-query
  [qmap {:keys [after] :as rq-params}
   {:keys [index-limit created-col] :as opts}]
  {:pre  [(map? qmap) (map? rq-params) (map? opts)
         (ah/map-has-keys? rq-params [:after])
         (ah/map-has-keys? opts [:index-limit :created-col :updated-col])]

   :post [(map? %) (ah/map-has-keys? % [:limit :created-col :sort-by-key
                                        :dir :query :query-args])]}
  (let [limit (min index-limit (get rq-params :limit index-limit))]
    (merge
     (select-keys opts [:created-col :updated-col])
     {:dir :newer :sort-by-key created-col :limit limit
      :query (qwrap-after-query qmap created-col (inc limit))
      :query-args {:after after}})))

(defn- qwrap-before-query
  [qmap created-col limit]
  (-> (hh/merge-where qmap [:<= created-col :?before])
      (hh/order-by [[created-col :desc]])
      (hh/limit limit)))

(defn- before-query
  [qmap
   {:keys [before] :as rq-params}
   {:keys [index-limit created-col] :as opts}]
  {:pre [(map? qmap) (map? opts)
         (ah/map-has-keys? rq-params [:before])
         (ah/map-has-keys? opts [:index-limit :created-col :updated-col])]

   :post [(map? %) (ah/map-has-keys? % [:limit :created-col :sort-by-key
                                        :dir :query :query-args])]}
  (let [limit (min index-limit (get rq-params :limit index-limit))]
    (merge
     (select-keys opts [:created-col :updated-col])
     {:dir :older :sort-by-key created-col :limit limit
      :query (qwrap-before-query qmap created-col (inc limit))
      :query-args {:before before}})))

(defn index-query
  "Takes a query, a map of supplied request parameters
  and returns a map containing the query and its arguments.
  If a deletion table query (for tables where resources can
  be deleted) are supplied, the final deletion query and its
  arguments are returned as well.

  query        -
  rq-params    - See 'Request Parameters'
  opts         - See 'Options' section.

  Request Parameters:
  'before'     -
  'after'      -
  'since'      -
  'limit'      - actual number of requested items - will by limited
                 by index-limit (see 'Options')

  Query Types:
  * Refresh Query  - Get entries who have changed since some timestamp
                     (':update-col' determines when the item was last modified)

  * Before-Query   - Get entries which are older than some date.
                     (':created-col ' determines entry age)

  * After-Query    - Get entries which are newer than some date
                     specified by the 'after' request parameter.

  NOTE: All queries except 'after-query' are organised from newest to oldest
        and every query is still subject to 'index' (request parameter) and
        'index-limit' (option).

  Options:
  :del-query   - a statement map containing the query to discover
                 deleted entries.
                 (default: nil)
  :index-limit - maximum number of entries to return in one 'page'
                 (default: 10)
  :created-col - name of the column in the queries table, holding
                 the timestamp of the entry's creation.
                 (default: :created_at)
  :updated-col - name of the column in the queried table, holding
                 the timestamp of the last time the entry was modified.
                 (default: :updated_at)
  :deleted-col - name of the column in the table tracking deletions
                 which holds the timestamp of the entry's deletion.
                 (default: deleted_at)

  Return:
  a map of queries and their query args (on success) or nil
  if the parameter combination employed aren't supported.

  return map keys:
  :query           - the (modified) statement map for fetching entries.
  :query-args      - arguments to be supplied the query.
  :del-query       - the (modified) statement map for fetching
                     records marking deleted entries.
  :del-query-args  - arguments to be supplied the deletion query."
  [query rq-params
   & {:keys [del-query] :as opts}]
  (let [opts (merge {:index-limit 10 :created-col :created_at
                     :updated-col :updated_at :deleted-col :deleted_at} opts)]
    (cond
      ;; since/refresh query
      (ah/map-has-keys? rq-params [:before :after :since])
      (if (nil? del-query)
        nil ;; unsupported query combination
        (refresh-query query del-query rq-params opts))

      ;; unsupported query combination
      (some #(ah/map-has-keys? rq-params %) [[:before :since] [:after :since] [:before :after]])
      nil

      ;; before-query
      (ah/map-has-keys? rq-params [:before])
      (before-query query rq-params opts)

      ;; after-query
      (ah/map-has-keys? rq-params [:after])
      (after-query query rq-params opts)

      true ;; default case
      (before-query query {:before (at/now-utc)} opts))))

(defn- has-more?
  "Compare number of results to the index-limit to determine
  if there are more results to return than this page."
  [results index-limit]
  (> (count results) index-limit))

(defn- qctx-dir
  [qctx]
  (get qctx :dir QCTX_DIR_DEFAULT))

(defn- qctx-dir-older?
  [qctx]
  (= (qctx-dir qctx) :older))

(defn- qctx-dir-newer?
  [qctx]
  (= (qctx-dir qctx) :newer))

(defn- adjust-dt
  "Adjust datetime by one second in the direction
  of the query sort direction (inc/dec time)."
  [dt qctx]
  (if (qctx-dir-newer? qctx)
    (t/plus dt (t/seconds 1))
    (t/minus dt (t/seconds 1))))

(defn rsp-map
  "Populate map m with :has-more? and :next, adjusting
  :data if has-more? is true.

  NOTE: will break if the number of entries are more than 1 greater
        than the index-limit (because 'next' date is referred from the last entry)"
  [m qctx]
  {:pre [(map? m) (map? qctx)
         (ah/map-has-keys? m [:data])
         (ah/map-has-keys? qctx [:limit :sort-by-key :dir])]

   :post [(map? %) (ah/map-has-keys? % [:data :has-more? :next])]}
  (let [get-last-result-date (fn [m qctx] (get (last (:data m)) (get qctx :sort-by-key)))]
    (cond (empty? (:data m))
          (assoc m :has-more? false
                 :next (-> (if (qctx-dir-newer? qctx)
                             (at/now-utc)
                             (at/unix-epoch-start))))
          (has-more? (:data m) (:limit qctx))
          (assoc m :has-more? true
                 :next (-> (get-last-result-date m qctx) (at/->datetime))
                 :data (butlast (:data m)))
          true ;; else, all results possible to get is in the resultset
          (assoc m :has-more? false
                 :next (-> (get-last-result-date m qctx) (at/->datetime) (adjust-dt qctx))))))
