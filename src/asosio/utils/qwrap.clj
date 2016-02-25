(ns asosio.utils.qwrap
  (:require [honeysql.core :as sql]
            [honeysql.helpers :as hh]))

(defn num-assets-query
  [{:keys [table fk fk-alias] :or {fk-alias fk}}]
  (-> (hh/select [fk fk-alias] [:%count.uid :num])
      (hh/from table)
      (hh/group fk)))

(defn with-stars
  "fetch number of stars for each item of the index.

  Args:
  :table    - table storing comments (e.g. :file_stars)
  :fk       - resource's FK (e.g. fid)
  :fk-alias - (if desired) alias the fk"
  [q {:keys [table fk fk-alias] :or {fk-alias fk} :as opts}]
  (let [sc-fk (keyword (str "sc." (name fk-alias)))]
    (-> q
        (hh/merge-select [(sql/raw "ifnull(sc.num, 0)") :num_stars])
        (hh/merge-left-join [(num-assets-query opts) :sc] [:= :id sc-fk]))))

(defn with-comments
  "fetch number of comments for each item of the index.

  Args:
  :table    - table storing comments (e.g. :file_comments)
  :fk       - resource's FK (e.g. fid)
  :fk-alias - (if desired) alias the fk"
  [q {:keys [table fk fk-alias] :or {fk-alias fk} :as opts}]
  (let [cc-fk (keyword (str "cc." (name fk-alias)))]
    (-> q
        (hh/merge-select [(sql/raw "ifnull(cc.num, 0)") :num_comments])
        (hh/merge-left-join [(num-assets-query opts) :cc] [:= :id cc-fk]))))
