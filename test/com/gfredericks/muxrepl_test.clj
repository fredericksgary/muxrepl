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

(deftest repl-rescue-test
  (let [f (fresh-session)]
    ;; deref a new promise, then push a new repl and deliver the
    ;; promise, pop the new repl and get the return value
    (is (= '[(var user/p)] (eval f (def p (promise)))))
    #_(eval f (clojure.core/println "Telempany"))
    (f {:op :eval, :code "[:dereferenced @p (spit \"oout\" \"tom\")]", :id "first"})
    (f {:op :eval, :code "#!push"})
    (is (= [42] (eval f @(deliver p 42))))
    (is (= [:dereferenced 42]
           ;; Again, we should figure out exactly what this damn
           ;; client code that produces this lazy seq is doing.
           (->> (f {:op :eval, :code "#!pop"})
                (filter #(= "first" (:id %)))
                (keep :value)
                (map read-string))))))
