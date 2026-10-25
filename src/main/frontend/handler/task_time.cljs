(ns frontend.handler.task-time
  "Renderer entry point for editing tracked task time."
  (:require [frontend.state :as state]))

(defn <edit-task-time!
  "Ask the worker to change the boundaries of one derived session. Resolves to
  {:ok true} or {:error code}."
  [task-uuid session changes]
  (state/<invoke-db-worker :thread-api/edit-task-time
                           (state/get-current-repo)
                           {:task-uuid task-uuid
                            :session session
                            :changes changes}))
