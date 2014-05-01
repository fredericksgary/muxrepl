(ns com.gfredericks.muxrepl.util)

(defn update
  [m k f & args]
  (apply update-in m [k] f args))
