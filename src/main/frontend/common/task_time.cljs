(ns frontend.common.task-time
  "Pure derivation and validation of tracked task time.

  A task has no persisted time-session entity. Tracked time is derived from the
  ordered status history of the task: each adjacent pair of status-history
  events either contributes an interval or does not. A history item is a map
  with :history-uuid, :created-at (epoch ms) and :status-ident (nil for custom
  statuses), ordered by :created-at.")

(def ^:private statuses-that-never-lead-to-counted-done
  #{:logseq.property/status.canceled
    :logseq.property/status.backlog
    :logseq.property/status.done})

(defn- active-session?
  [cur last?]
  (and last? (= :logseq.property/status.doing (:status-ident cur))))

(defn- counted-pair?
  [prev cur]
  (let [prev-status (:status-ident prev)]
    (or (= prev-status :logseq.property/status.doing)
        (and (not (contains? statuses-that-never-lead-to-counted-done prev-status))
             (= (:status-ident cur) :logseq.property/status.done)))))

(defn- pair->session
  [now-ms prev cur last?]
  (cond
    (active-session? cur last?)
    (let [start-ms (:created-at cur)]
      {:start-history-uuid (:history-uuid cur)
       :start-ms start-ms
       :seconds (quot (- now-ms start-ms) 1000)
       :active? true})

    (counted-pair? prev cur)
    (let [start-ms (:created-at prev)
          end-ms (:created-at cur)]
      {:start-history-uuid (:history-uuid prev)
       :end-history-uuid (:history-uuid cur)
       :start-ms start-ms
       :end-ms end-ms
       :seconds (quot (- end-ms start-ms) 1000)
       :active? false})))

(defn- session-ms
  [now-ms {:keys [start-ms end-ms]}]
  (- (or end-ms now-ms) start-ms))

(defn derive-time
  "Derive tracked sessions and total seconds from ordered status `history`.

  `now-ms` is the clock used for an active Doing session. Both the sessions and
  the total come from the same intervals; the total is floored once from the
  summed milliseconds, so it can exceed the sum of per-session seconds by
  rounding."
  [history now-ms]
  (let [last-idx (dec (count history))
        sessions (into []
                       (keep identity)
                       (map-indexed (fn [idx [prev cur]]
                                      (pair->session now-ms prev cur (= (inc idx) last-idx)))
                                    (partition 2 1 history)))]
    {:sessions sessions
     :seconds (quot (transduce (map #(session-ms now-ms %)) + 0 sessions) 1000)}))

(defn- find-session
  [sessions {:keys [start-history-uuid end-history-uuid]}]
  (some (fn [session]
          (when (and (= start-history-uuid (:start-history-uuid session))
                     (= end-history-uuid (:end-history-uuid session)))
            session))
        sessions))

(defn- valid-timestamp?
  [value]
  (and (integer? value)
       (not (neg? value))
       (<= value js/Number.MAX_SAFE_INTEGER)))

(defn- history-index
  [history history-uuid]
  (first (keep-indexed (fn [idx item]
                         (when (= history-uuid (:history-uuid item))
                           idx))
                       history)))

(defn- ordering-error
  [history' session indexes]
  (let [start-idx (history-index history' (:start-history-uuid session))
        end-idx (some->> (:end-history-uuid session) (history-index history'))
        created-at #(:created-at (nth history' %))]
    (if (and end-idx (>= (created-at start-idx) (created-at end-idx)))
      :start-not-before-end
      (when (some (fn [idx]
                    (let [prev-idx (dec idx)
                          next-idx (inc idx)]
                      (or (and (>= prev-idx 0)
                               (<= (created-at idx) (created-at prev-idx)))
                          (and (< next-idx (count history'))
                               (>= (created-at idx) (created-at next-idx))))))
                  indexes)
        :crosses-neighbor))))

(defn validate-edit
  "Validate changing the boundaries of one derived session.

  `session` identifies the session by :start-history-uuid and, for a completed
  session, :end-history-uuid. `changes` is a non-empty vector of
  {:history-uuid :created-at}, each naming one of the session's boundaries.
  Malformed requests throw; outcomes a user can correct return {:error code}.
  Otherwise returns {:updates [{:history-uuid :created-at}]} holding only
  boundaries whose timestamp actually changes."
  [{:keys [history now-ms session changes]}]
  (when (empty? changes)
    (throw (ex-info "Task time edit requires changes" {:changes changes})))
  (when (not= (count changes) (count (distinct (map :history-uuid changes))))
    (throw (ex-info "Task time edit repeats a history item" {:changes changes})))
  (let [{:keys [sessions]} (derive-time history now-ms)
        session' (find-session sessions session)
        boundary-uuids (when session'
                         (into #{} (keep session') [:start-history-uuid :end-history-uuid]))]
    (cond
      (nil? session')
      {:error :session-not-found}

      (not (every? #(contains? boundary-uuids (:history-uuid %)) changes))
      {:error :boundary-not-in-session}

      (not (every? #(valid-timestamp? (:created-at %)) changes))
      {:error :invalid-timestamp}

      :else
      (let [by-uuid (into {} (map (juxt :history-uuid identity)) history)
            updates (filterv #(not= (:created-at %)
                                    (:created-at (get by-uuid (:history-uuid %))))
                             changes)
            new-created-at (into {} (map (juxt :history-uuid :created-at)) updates)
            history' (mapv #(assoc % :created-at (get new-created-at (:history-uuid %)
                                                      (:created-at %)))
                           history)
            error (when (seq updates)
                    (ordering-error history' session'
                                    (map #(history-index history' (:history-uuid %)) updates)))]
        (cond
          (empty? updates) {:error :no-change}
          error {:error error}
          (and (:active? session')
               (>= (get new-created-at (:start-history-uuid session')) now-ms))
          {:error :start-in-future}
          :else {:updates (mapv #(select-keys % [:history-uuid :created-at]) updates)})))))
