(ns com.gfredericks.muxrepl-test
  (:refer-clojure :exclude [eval])
  (:require [clojure.tools.nrepl :as client]
            [clojure.tools.nrepl.server :as server])
  (:use clojure.test
        com.gfredericks.muxrepl))

(def ^:dynamic *client*)

(defn server-fixture
  [test]
  (with-open [server (server/start-server
                      :port 56408
                      :bind "127.0.0.1"
                      :handler (server/default-handler #'wrap-muxrepl))
              t (client/connect :port 56408 :host "127.0.0.1")]
    (let [c (client/client t 100)]
      (binding [*client* c]
        (test)))))

(defn clear-session-info-fixture
  [test]
  (dosync
   (alter session-owners (constantly {}))
   (alter active-session (constantly {})))
  (test))

(use-fixtures :each server-fixture clear-session-info-fixture)

(defn fresh-session
  []
  (client/client-session *client*))

(defn eval*
  [session-fn code-string]
  (->> (session-fn {:op :eval, :code code-string})
       (map (fn [{:keys [ex err] :as msg}]
              (if (or ex err)
                (throw (ex-info (str "Error during eval!")
                                {:code code-string
                                 :msg msg}))
                msg)))
       (keep :value)
       (map read-string)
       (doall)))

(defmacro eval
  "Returns a sequence of return values from the evaluation."
  [session-fn eval-code]
  `(eval* ~session-fn (client/code ~eval-code)))

(deftest hello-world-test
  (let [f (fresh-session)]
    (is (= [42] (eval f (* 2 3 7)))
        "I can run code.")))
