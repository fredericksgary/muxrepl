(ns com.gfredericks.muxrepl.util)

(defn update
  [m k f & args]
  (apply update-in m [k] f args))

(defn queue
  ([] clojure.lang.PersistentQueue/EMPTY)
  ([& xs] (into (queue) xs)))
