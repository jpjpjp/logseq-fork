(ns frontend.worker.handler.task-time
  "Task time sessions derived from status history, and the narrow mutation that
  corrects a session's boundary timestamps."
  (:require [datascript.core :as d]
            [frontend.common.task-time :as task-time]
            [frontend.common.thread-api :refer [def-thread-api]]
            [frontend.worker.state :as worker-state]
            [logseq.common.util :as common-util]
            [logseq.outliner.op :as outliner-op]))

(defn status-history
  "Ordered status-history items of the task entity `task-id`."
  [db task-id]
  (->> (d/q '[:find ?history ?created-at ?status
              :in $ ?task-id
              :where
              [?history :logseq.property.history/block ?task-id]
              [?history :logseq.property.history/property :logseq.property/status]
              [?history :logseq.property.history/ref-value ?status]
              [?history :block/created-at ?created-at]]
            db
            task-id)
       (sort-by (juxt second first))
       (mapv (fn [[history-id created-at status-id]]
               {:history-uuid (:block/uuid (d/entity db history-id))
                :created-at created-at
                :status-ident (:db/ident (d/entity db status-id))
                :status-uuid (:block/uuid (d/entity db status-id))}))))

(defn- history-entity-error
  [db task history-uuid]
  (let [history (d/entity db [:block/uuid history-uuid])]
    (cond
      (nil? history) :history-not-found
      (not= (:db/id task) (:db/id (:logseq.property.history/block history))) :history-not-in-task
      (not= :logseq.property/status
            (:db/ident (:logseq.property.history/property history))) :history-not-in-task)))

(defn edit-task-time!
  "Change the :block/created-at of the status-history events bounding one
  derived session of a task, in one transaction through the outliner path so
  persistence, sync and undo/redo apply as for any local edit.

  `request` is {:task-uuid :session {:start-history-uuid :end-history-uuid}
  :changes [{:history-uuid :created-at}]}. Returns {:ok true} or {:error code}
  for outcomes a user can correct (stale session, ordering, ownership)."
  [conn {:keys [task-uuid session changes]} now-ms]
  (let [db @conn
        task (or (d/entity db [:block/uuid task-uuid])
                 (throw (ex-info "Missing task for time edit" {:task-uuid task-uuid})))
        ownership-error (some #(history-entity-error db task (:history-uuid %)) changes)
        {:keys [error updates]}
        (if ownership-error
          {:error ownership-error}
          (task-time/validate-edit {:history (status-history db (:db/id task))
                                    :now-ms now-ms
                                    :session session
                                    :changes changes}))]
    (if error
      {:error error}
      (do
        (outliner-op/apply-ops!
         conn
         [[:transact [(mapv (fn [{:keys [history-uuid created-at]}]
                              [:db/add [:block/uuid history-uuid] :block/created-at created-at])
                            updates)
                      nil]]]
         nil)
        {:ok true}))))

(def-thread-api :thread-api/edit-task-time
  [repo request]
  (let [conn (or (worker-state/get-datascript-conn repo)
                 (throw (ex-info "Missing worker graph connection" {:repo repo})))]
    (worker-state/set-db-latest-tx-time! repo)
    (edit-task-time! conn request (common-util/time-ms))))
