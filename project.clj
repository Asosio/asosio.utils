(defproject asosio.utils "0.0.4"
  :description "The Asosio Utils is a collection of utilities that makes development of clojure webservices easier."
  :url "http://asosio.com"
  :license {:name "BSD 3-Clause License"
            :url "https://opensource.org/licenses/BSD-3-Clause"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [funcool/clojure.jdbc "0.6.1"]
                 [mysql/mysql-connector-java "5.1.6"]
                 [com.mchange/c3p0 "0.9.5.1"]
                 [ring/ring-core "1.4.0"]
                 [honeysql "0.6.1"]
                 [com.taoensso/timbre "4.1.1"]
                 [com.novemberain/validateur "2.4.2"]
                 [org.clojure/algo.monads "0.1.5"]
                 [clj-jwt "0.1.1"]
                 [environ "1.0.1"]
                 [clj-time "0.11.0"]])
