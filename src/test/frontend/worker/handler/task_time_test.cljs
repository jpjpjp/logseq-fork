(ns frontend.worker.handler.task-time-test
  (:require [cljs.test :refer [deftest is testing]]
            [datascript.core :as d]
            [frontend.common.task-time :as task-time]
            [frontend.worker.handler.query :as query-handler]
            [frontend.worker.handler.task-time :as task-time-handler]
            [logseq.db.test.helper :as db-test]))

(def ^:private status-idents
  {:todo :logseq.property/status.todo
   :doing :logseq.property/status.doing
   :done :logseq.property/status.done
   :canceled :logseq.property/status.canceled
   :backlog :logseq.property/status.backlog})

(defn- create-task!
  "Create a task block and its status history from [created-at status] pairs.
  A status is a key of status-idents or :custom. Returns {:conn :task :history}
  where history holds the history uuids in the given order."
  [conn title entries]
  (let [custom-uuid (random-uuid)
        history-uuids (mapv (fn [_] (random-uuid)) entries)
        task-uuid (random-uuid)
        page (db-test/find-page-by-title @conn "Page")
        custom-status-tx {:db/id -1
                          :block/uuid custom-uuid
                          :block/title "Paused"
                          :logseq.property/created-from-property
                          :logseq.property/status}]
    (d/transact! conn
                 (into [custom-status-tx
                        {:db/id -2
                         :block/uuid task-uuid
                         :block/title title
                         :block/page (:db/id page)
                         :block/parent (:db/id page)
                         :block/order (str "a" (random-uuid))}]
                       (map (fn [history-uuid [created-at status]]
                              {:block/uuid history-uuid
                               :block/created-at created-at
                               :logseq.property.history/block -2
                               :logseq.property.history/property :logseq.property/status
                               :logseq.property.history/ref-value
                               (if (= :custom status) -1 (get status-idents status))})
                            history-uuids entries)))
    {:conn conn :task task-uuid :history history-uuids}))

(defn- new-conn
  []
  (db-test/create-conn-with-blocks
   {:pages-and-blocks [{:page {:block/title "Page"}}]}))

(defn- task-eid
  [conn task-uuid]
  (:db/id (d/entity @conn [:block/uuid task-uuid])))

(defn- derived
  [conn task-uuid now-ms]
  (task-time/derive-time (task-time-handler/status-history @conn (task-eid conn task-uuid))
                         now-ms))

(defn- reference-spent-seconds
  "The counting loop task-spent-time used before it delegated to
  task-time/derive-time, kept as an independent oracle."
  [history now-ms]
  (loop [[last-item item & others] history
         time 0]
    (if item
      (let [last-status (:status-ident last-item)
            this-status (:status-ident item)]
        (if (and (= this-status :logseq.property/status.doing)
                 (empty? others))
          (quot (+ time (- now-ms (:created-at item))) 1000)
          (recur (cons item others)
                 (if (or (= last-status :logseq.property/status.doing)
                         (and (not (contains? #{:logseq.property/status.canceled
                                                :logseq.property/status.backlog
                                                :logseq.property/status.done} last-status))
                              (= this-status :logseq.property/status.done)))
                   (+ time (- (:created-at item) (:created-at last-item)))
                   time))))
      (quot time 1000))))

(deftest derivation-matches-the-existing-task-spent-time-calculation-test
  (let [now-ms 100000
        cases {"single doing" [[1000 :doing]]
               "doing then done" [[1000 :doing] [4000 :done]]
               "direct todo to done" [[1000 :todo] [6000 :done]]
               "custom to done" [[1000 :custom] [6000 :done]]
               "custom to todo" [[1000 :custom] [6000 :todo]]
               "backlog to done" [[1000 :backlog] [6000 :done]]
               "canceled to done" [[1000 :canceled] [6000 :done]]
               "done to done" [[1000 :done] [6000 :done]]
               "two cycles" [[0 :todo] [1000 :doing] [3000 :todo] [5000 :doing] [9500 :done]]
               "completed then reopened and active" [[0 :todo] [1000 :doing] [4000 :done] [6000 :doing]]
               "doing to doing active" [[1000 :doing] [3000 :doing]]
               "doing to custom to doing" [[1000 :doing] [2000 :custom] [3000 :doing]]
               "sub-second remainders" [[0 :doing] [900 :todo] [1000 :doing] [1900 :done]]
               "active only after todo" [[0 :todo] [1000 :doing]]}]
    (doseq [[label entries] cases]
      (testing label
        (let [{:keys [conn task]} (create-task! (new-conn) label entries)
              history (task-time-handler/status-history @conn (task-eid conn task))
              [_ query-seconds] (query-handler/task-spent-time @conn (task-eid conn task) now-ms)
              seconds (:seconds (derived conn task now-ms))]
          (is (= (reference-spent-seconds history now-ms) seconds))
          (is (= query-seconds seconds)))))))

(deftest no-history-is-untracked-test
  (let [{:keys [conn task]} (create-task! (new-conn) "none" [])]
    (is (= {:sessions [] :seconds 0} (derived conn task 5000)))))

(defn- created-at
  [conn history-uuid]
  (:block/created-at (d/entity @conn [:block/uuid history-uuid])))

(defn- edit!
  [conn task session changes now-ms]
  (task-time-handler/edit-task-time! conn {:task-uuid task :session session :changes changes} now-ms))

(deftest doing-without-history-is-a-valid-untracked-state-test
  ;; The File-to-DB importer sets the current status directly and creates no
  ;; status-history entities, so Doing does not imply a recorded start.
  (let [{:keys [conn task]} (create-task! (new-conn) "imported doing" [])
        task-id (task-eid conn task)
        _ (d/transact! conn [{:db/id task-id
                              :logseq.property/status :logseq.property/status.doing}])
        history (task-time-handler/status-history @conn task-id)
        status #(:db/ident (:logseq.property/status (d/entity @conn task-id)))
        absent-history (random-uuid)]
    (is (= :logseq.property/status.doing (status)))
    (is (= [] history))
    (is (= {:sessions [] :seconds 0} (task-time/derive-time history 999999))
        "No session is invented from the task's creation or import time.")
    (testing "requests naming a missing session or boundary are correctable, not thrown"
      (is (= {:error :history-not-found}
             (edit! conn task {:start-history-uuid absent-history}
                    [{:history-uuid absent-history :created-at 500}] 999999)))
      (is (= {:error :history-not-in-task}
             (edit! conn task {:start-history-uuid task}
                    [{:history-uuid task :created-at 500}] 999999))))
    (is (= :logseq.property/status.doing (status)) "the current status is untouched")
    (is (= [] (task-time-handler/status-history @conn task-id)) "no history is manufactured")))

(deftest edit-changes-only-the-selected-boundaries-test
  (let [{:keys [conn task history]} (create-task! (new-conn) "two sessions"
                                                  [[0 :todo] [1000 :doing] [5000 :done]
                                                   [7000 :doing] [9000 :todo]])
        [_ h-start h-end h2-start h2-end] history
        title-before (:block/title (d/entity @conn [:block/uuid task]))
        status-before (mapv :status-uuid (task-time-handler/status-history @conn (task-eid conn task)))]
    (testing "moving the start earlier increases the duration by the exact amount"
      (is (= {:ok true}
             (edit! conn task {:start-history-uuid h-start :end-history-uuid h-end}
                    [{:history-uuid h-start :created-at 500}] 99999)))
      (is (= [500 5000 7000 9000]
             (mapv #(created-at conn %) [h-start h-end h2-start h2-end])))
      (is (= 6 (:seconds (derived conn task 99999)))))
    (testing "moving the end earlier decreases it"
      (is (= {:ok true}
             (edit! conn task {:start-history-uuid h-start :end-history-uuid h-end}
                    [{:history-uuid h-end :created-at 3000}] 99999)))
      (is (= 4 (:seconds (derived conn task 99999)))))
    (testing "both boundaries apply together and other sessions are preserved"
      (is (= {:ok true}
             (edit! conn task {:start-history-uuid h-start :end-history-uuid h-end}
                    [{:history-uuid h-start :created-at 1000}
                     {:history-uuid h-end :created-at 6000}] 99999)))
      (is (= [5 2] (mapv :seconds (:sessions (derived conn task 99999)))))
      (is (= [7000 9000] (mapv #(created-at conn %) [h2-start h2-end]))))
    (testing "status values, history identities and task title are unchanged"
      (is (= title-before (:block/title (d/entity @conn [:block/uuid task]))))
      (is (= status-before
             (mapv :status-uuid (task-time-handler/status-history @conn (task-eid conn task)))))
      (is (= history (mapv :history-uuid (task-time-handler/status-history @conn (task-eid conn task))))))))

(deftest edit-rejections-leave-data-unchanged-test
  (let [{:keys [conn task history]} (create-task! (new-conn) "rejects"
                                                  [[0 :todo] [1000 :doing] [5000 :done]
                                                   [7000 :doing] [9000 :todo]])
        [_ h-start h-end h2-start _] history
        session {:start-history-uuid h-start :end-history-uuid h-end}
        before (task-time-handler/status-history @conn (task-eid conn task))
        other-task (create-task! conn "other" [[100 :doing] [200 :done]])
        [other-doing] (:history other-task)
        before' (task-time-handler/status-history @conn (task-eid conn task))
        reject (fn [session changes expected]
                 (is (= {:error expected} (edit! conn task session changes 99999))
                     (pr-str changes)))]
    (reject session [{:history-uuid h-start :created-at 5000}] :start-not-before-end)
    (reject session [{:history-uuid h-end :created-at 7000}] :crosses-neighbor)
    (reject session [{:history-uuid h-start :created-at -5}] :invalid-timestamp)
    (reject session [{:history-uuid h-start :created-at 1000}] :no-change)
    (reject session [{:history-uuid h2-start :created-at 7500}] :boundary-not-in-session)
    (reject {:start-history-uuid h-start :end-history-uuid h2-start}
            [{:history-uuid h-start :created-at 500}] :session-not-found)
    (reject session [{:history-uuid (random-uuid) :created-at 500}] :history-not-found)
    (testing "a history entity of another task is rejected, even with a valid timestamp"
      (reject session [{:history-uuid other-doing :created-at 150}] :history-not-in-task)
      (is (= 100 (created-at conn other-doing))))
    (testing "the task block itself is not a status-history entity"
      (reject session [{:history-uuid task :created-at 500}] :history-not-in-task))
    (is (= before before'))
    (is (= before (task-time-handler/status-history @conn (task-eid conn task))))
    (testing "a missing task is a programmer error"
      (is (thrown? js/Error (edit! conn (random-uuid) session
                                   [{:history-uuid h-start :created-at 500}] 99999))))))

(deftest edit-active-session-start-test
  (let [{:keys [conn task history]} (create-task! (new-conn) "active"
                                                  [[0 :todo] [1000 :doing]])
        [_ h-doing] history
        session {:start-history-uuid h-doing}]
    (is (= {:ok true} (edit! conn task session [{:history-uuid h-doing :created-at 500}] 8000)))
    (is (= 7 (:seconds (derived conn task 8000))))
    (is (= {:error :start-in-future}
           (edit! conn task session [{:history-uuid h-doing :created-at 9000}] 8000)))
    (is (= 500 (created-at conn h-doing)))))

(deftest edit-transacts-through-the-outliner-path-test
  (let [{:keys [conn task history]} (create-task! (new-conn) "tx"
                                                  [[1000 :doing] [5000 :done]])
        [h-start h-end] history
        *tx-reports (atom [])]
    (d/listen! conn ::capture #(swap! *tx-reports conj %))
    (edit! conn task {:start-history-uuid h-start :end-history-uuid h-end}
           [{:history-uuid h-start :created-at 2000}
            {:history-uuid h-end :created-at 4000}] 99999)
    (d/unlisten! conn ::capture)
    (is (= 1 (count @*tx-reports)) "both boundaries land in one transaction")
    (let [{:keys [tx-data tx-meta]} (first @*tx-reports)]
      (is (true? (:local-tx? tx-meta)))
      (is (some? (:outliner-op tx-meta)))
      (is (= #{:block/created-at}
             (disj (set (map :a tx-data)) :block/updated-at :block/tx-id))))))

(deftest edit-backdates-the-session-start-and-shifts-the-creation-event-test
  (let [{:keys [conn task history]} (create-task! (new-conn) "backdated"
                                                  [[50000000 :todo] [60000000 :doing] [60010000 :done]])
        [h-todo h-start h-end] history
        session {:start-history-uuid h-start :end-history-uuid h-end}
        new-start 10000000]
    (is (= {:ok true}
           (edit! conn task session [{:history-uuid h-start :created-at new-start}] 99999999)))
    (is (= (- new-start 1000) (created-at conn h-todo)))
    (is (= new-start (created-at conn h-start)))
    (is (= 60010000 (created-at conn h-end)))
    (is (= [:logseq.property/status.todo :logseq.property/status.doing :logseq.property/status.done]
           (mapv :status-ident (task-time-handler/status-history @conn (task-eid conn task)))))
    (is (= (quot (- 60010000 new-start) 1000) (:seconds (derived conn task 99999999))))))
