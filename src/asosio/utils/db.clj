(ns asosio.utils.db
  (:require [jdbc.core :as jdbc]
            [honeysql.core :as sql]
            [honeysql.helpers :as hh]
            [taoensso.timbre :as timbre]
            [asosio.utils.dbtypes])) ; automatic coercion of datetimes

(defn- format-query
  "Construct SQL query from a statement map (honeysql) and
  a map of parameters.
  fn-name - name of calling function (used for logging).
  stmt-map - statement map (built with honeysql)
  stmt-params - (OPTIONAL) parameters to be inserted into stmt-map query"
  ([fn-name stmt-map] (format-query fn-name stmt-map {}))
  ([fn-name stmt-map stmt-params]
   (let [query (sql/format stmt-map :params stmt-params)]
     (timbre/debug fn-name ":\n   ---\n   Query:" query "\n   stmt-map: "
                   stmt-map "\n   stmt-params: " stmt-params"\n")
     query)))

(defmacro with-tx
  "Open a DB connection and wrap 'body' as an
  atomic action (a transaction)."
  [[c conn] & body]
  `(with-open [~'c (jdbc/connection ~conn)]
     (jdbc/atomic ~'c ~@body)))

;; TODO - implement a CAP functionality (like old-school Goooooooooogle)
(defn count-num-rows
  "Get number of rows returned by executing the query.

  NOTE - count() should only use columnds for which there's an
  index. In such cases, the value is derived without a full
  table scan. sql_calc_found_rows() should be used otherwise."
  ([conn query-map] (count-num-rows conn query-map {}))
  ([conn query-map query-params]
   (-> (jdbc/fetch conn
                   (format-query
                    "count-num-rows"
                    (-> (dissoc query-map :select :limit)
                        (hh/select [:%count.* :count]))
                    query-params))
       (first)
       (get :count))))

(defn execute
  "Helper function to execute a (possibly parameterised) statement.
  Used for non-queries, typically DELETE statements.

  Tales a connection (can be wrapped in a transaction), a
  statement map (honeysql) and optionally, a set of query
  parameters. Returns the number of rows affected."
  ([conn stmt-map] (execute conn stmt-map {}))
  ([conn stmt-map stmt-params]
   (->> (format-query "execute" stmt-map stmt-params)
        (jdbc/execute conn))))

(defn fetch
  "Helper function to execute a (possible parameterised) query.

  Takes a connection (can be wrapped in a transaction), a
  statement map (honeysql) and optionally, a set of query
  parameters. Returns a vector of maps, one for each match."
  ([conn stmt-map] (execute conn stmt-map {}))
  ([conn stmt-map stmt-params]
   (->> (format-query "fetch" stmt-map stmt-params)
        (jdbc/fetch conn))))

(defn insert-record
  "Helper function to execute a honeysql insert query

  Will return a non-nil value provided the row inserted
  would have lead to a primary key being generated.
  If a PK is explicitly set (=> isn't auto-incremented)
  then nil is returned - it is presumed the caller knows
  which of the columns is the primary key."
  ([conn stmt-map] (insert-record conn stmt-map {}))
  ([conn stmt-map stmt-params]
  (jdbc/atomic conn
   (jdbc/execute conn (format-query "insert-record" stmt-map stmt-params))
   (let [id (->> "select last_insert_id() as id"
                 (jdbc/fetch conn)
                 (first)
                 (:id))]
     (if (zero? id)
       nil
       id)))))
