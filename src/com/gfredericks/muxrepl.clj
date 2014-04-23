(ns com.gfredericks.muxrepl
  (:require [clojure.tools.nrepl.middleware :refer [set-descriptor!]]
            [clojure.tools.nrepl.misc :refer [response-for]]
            [clojure.tools.nrepl.transport :as transport]))

(def prefix "#!")

(defn set-prefix!
  "Sets the string prefix used to identify a special muxrepl command."
  [s]
  (alter-var-root #'prefix (constantly s)))

(defonce ^{:doc "A map from internal session IDs to external session IDs."}
  session-owners
  (ref {}))

(defonce ^{:doc "A map from external session IDs to internal session IDs."}
  active-session
  (ref {}))

(defn ^:private wrap-transport
  "Wraps the :transport so that all outgoing messages are passed
  through the function f."
  [msg f]
  (update-in msg [:transport]
             (fn [t]
               (reify transport/Transport
                 (send [this msg]
                   (transport/send t (f msg)))))))

(defn wrap-muxrepl-dispatch
  [handler]
  (fn [msg]
    (if (= "clone" (:op msg))
      (-> msg
          (wrap-transport (fn [out-msg]
                            (when-let [new-session (:new-session out-msg)]
                              (dosync
                               (alter active-session assoc new-session new-session)
                               (alter session-owners assoc new-session new-session)))
                            out-msg))
          (handler))
      (-> msg
          (update-in [:session]
                     (fn [external-session]
                       {:post [%]}
                       (@active-session external-session)))
          (handler)))))

(defn wrap-muxrepl-api
  [handler]
  handler)

(defn wrap-muxrepl
  [handler]
  (-> handler
      (wrap-muxrepl-dispatch)
      (wrap-muxrepl-api)))

(set-descriptor! #'wrap-muxrepl
                 {:expects #{"eval" "clone"
                             #'clojure.tools.nrepl.middleware.session/session}})
