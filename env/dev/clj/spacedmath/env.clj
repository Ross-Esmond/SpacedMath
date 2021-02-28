(ns spacedmath.env
  (:require
    [selmer.parser :as parser]
    [clojure.tools.logging :as log]
    [spacedmath.dev-middleware :refer [wrap-dev]]))

(def defaults
  {:init
   (fn []
     (parser/cache-off!)
     (log/info "\n-=[spacedmath started successfully using the development profile]=-"))
   :stop
   (fn []
     (log/info "\n-=[spacedmath has shut down successfully]=-"))
   :middleware wrap-dev})
