(ns com.gfredericks.muxrepl
  (:require [clojure.tools.nrepl.middleware :refer [set-descriptor!]]
            [clojure.tools.nrepl.misc :refer [response-for]]
            [clojure.tools.nrepl.transport :as transport]))

(defn ^:private update
  [m k f & args]
  (apply update-in m [k] f args))

(def prefix "#!")

(defn set-prefix!
  "Sets the string prefix used to identify a special muxrepl command."
  [s]
  (alter-var-root #'prefix (constantly s)))

(defonce ^{:doc "A map from internal session IDs to external session IDs."}
  session-owners
  (ref {}))

(defonce ^{:doc
"A map from external session IDs to a stack of internal session IDs.
The topmost element on the stack is the active session id."}
  active-session
  (ref {}))

(def ^:private dev-null (reify transport/Transport (send [_ _])))

(defn ^:private wrap-transport
  "Wraps the :transport so that all outgoing messages are passed
  through the function f."
  [msg f]
  (update msg :transport
          (fn [t]
            (reify transport/Transport
              (send [this msg]
                (transport/send t (f msg)))))))

(defn clone-session
  "Returns the new session id."
  [handler session-id]
  (let [p (promise)]
    (handler {:op "clone"
              :session session-id
              :transport (reify transport/Transport
                           (send [this out-msg]
                             (deliver p (:new-session out-msg))))})
    @p))

(defn close-session
  [handler session-id]
  (handler {:op "close"
            :session session-id
            :transport dev-null}))

(defn remote-println
  [msg & args]
  (transport/send (:transport msg)
                  (response-for msg
                                ;; is this the right key?
                                {:out (apply println-str args)})))

(defn parse-command
  [s]
  (subs s (count prefix)))

(defmulti handle-muxrepl-command
  (fn [_handler {:keys [code]}]
    (parse-command code)))

(defmethod handle-muxrepl-command "push"
  [handler {external-session :session, :as msg}]
  (remote-println msg "Pushing new repl...")
  (let [new-session-id (clone-session handler external-session)]
    (dosync
     (alter session-owners assoc new-session-id external-session)
     (alter active-session update external-session conj new-session-id))))

(defmethod handle-muxrepl-command "pop"
  [handler {external-session :session, :as msg}]
  (let [a (agent nil)]
    (dosync
     (let [internal-sessions (@active-session external-session)]
       (if (= 1 (count internal-sessions))
         (send a (fn [_] (remote-println msg "No repl to pop!")))
         (let [session-to-close (peek internal-sessions)]
           (do (alter session-owners dissoc session-to-close)
               (alter active-session update external-session pop)
               (send a (fn [_]
                         (remote-println msg "Popped current repl")
                         (close-session handler session-to-close))))))))))

(defmethod handle-muxrepl-command :default
  [handler msg]
  (remote-println msg (str "Unknown muxrepl command: " (parse-command (:code msg)))))

(defn wrap-muxrepl-dispatch
  [handler]
  (fn [msg]
    ;; TODO: handle closing sessions
    ;; TODO: find out what ls-sessions actually does
    (if (= "clone" (:op msg))
      (-> msg
          (wrap-transport (fn [out-msg]
                            (when-let [new-session (:new-session out-msg)]
                              (dosync
                               (alter active-session update new-session conj new-session)
                               (alter session-owners assoc new-session new-session)))
                            out-msg))
          (handler))
      (-> msg
          (update :session
                  (fn [external-session]
                    {:post [%]}
                    (peek (@active-session external-session))))
          (wrap-transport (fn [out-msg]
                            (assoc out-msg :session (:session msg))))
          (handler)))))

(defn wrap-muxrepl-api
  [handler]
  (fn [msg]
    (if (and (= "eval" (:op msg))
             (.startsWith (:code msg) prefix))
      (handle-muxrepl-command handler msg)
      (handler msg))))

(defn wrap-muxrepl
  [handler]
  (-> handler
      (wrap-muxrepl-dispatch)
      (wrap-muxrepl-api)))

(set-descriptor! #'wrap-muxrepl
                 {:expects #{"eval" "clone"
                             #'clojure.tools.nrepl.middleware.session/session}})
