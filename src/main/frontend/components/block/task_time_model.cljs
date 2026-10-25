(ns frontend.components.block.task-time-model
  "Pure view-model helpers for the task time session editor: local date-time
  formatting and parsing, and building a validated edit request."
  (:require [clojure.string :as string]
            [frontend.common.task-time :as task-time]))

(defn- pad
  [n width]
  (let [s (str n)]
    (str (string/join (repeat (max 0 (- width (count s))) "0")) s)))

(defn- local-parts
  [ms]
  (let [d (js/Date. ms)]
    {:date (str (pad (.getFullYear d) 4) "-" (pad (inc (.getMonth d)) 2) "-" (pad (.getDate d) 2))
     :time (str (pad (.getHours d) 2) ":" (pad (.getMinutes d) 2) ":" (pad (.getSeconds d) 2))}))

(defn format-local-input
  "Epoch ms as a local `YYYY-MM-DDTHH:mm:ss` datetime-local value."
  [ms]
  (let [{:keys [date time]} (local-parts ms)]
    (str date "T" time)))

(defn format-local-display
  "Epoch ms as local `YYYY-MM-DD HH:mm:ss` text."
  [ms]
  (let [{:keys [date time]} (local-parts ms)]
    (str date " " time)))

(def ^:private local-input-re
  #"^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2})(?::(\d{2}))?$")

(def ^:private dst-shift-ms [1800000 3600000])

(defn- ambiguous-local-time?
  "True when a nearby instant displays the same local text, i.e. the time falls
  in a repeated hour at a daylight-saving change."
  [ms input-value]
  (some (fn [shift]
          (or (= input-value (format-local-input (+ ms shift)))
              (= input-value (format-local-input (- ms shift)))))
        dst-shift-ms))

(defn parse-local-input
  "Parse a local datetime-local value to epoch ms, or nil when the text is
  malformed, names a local time that does not exist (daylight-saving gap), or
  names one that occurs twice (daylight-saving overlap)."
  [value]
  (when-let [[_ y mo d h mi s] (and (string? value) (re-matches local-input-re value))]
    (let [[y mo d h mi s] (map #(if % (js/parseInt % 10) 0) [y mo d h mi s])
          date (js/Date. y (dec mo) d h mi s)
          ms (.getTime date)
          canonical (format-local-input ms)
          expected (str (pad y 4) "-" (pad mo 2) "-" (pad d 2) "T" (pad h 2) ":" (pad mi 2) ":" (pad s 2))]
      (when (and (not (js/isNaN ms))
                 (= canonical expected)
                 (not (ambiguous-local-time? ms expected)))
        ms))))

(defn session-view
  "Display data for one derived session."
  [{:keys [start-history-uuid end-history-uuid start-ms end-ms seconds active?]}]
  {:key (str start-history-uuid "-" end-history-uuid)
   :start-history-uuid start-history-uuid
   :end-history-uuid end-history-uuid
   :seconds seconds
   :active? (boolean active?)
   :start-text (format-local-display start-ms)
   :end-text (when-not active? (format-local-display end-ms))
   :start-input (format-local-input start-ms)
   :end-input (when-not active? (format-local-input end-ms))})

(defn- boundary-change
  [history-uuid original-input new-input]
  (when (not= original-input new-input)
    {:history-uuid history-uuid
     :created-at (parse-local-input new-input)}))

(defn build-edit
  "Build the worker request session and changes from the editor inputs.

  Returns {:session :changes} or {:error code}, running the same validation the
  worker repeats. Only boundaries whose input differs from the session's
  displayed value are changed, so untouched sub-second precision is kept."
  [{:keys [history now-ms start-input end-input] view :session-view}]
  (let [{:keys [start-history-uuid end-history-uuid active?]} view
        changes (into []
                      (keep identity)
                      [(boundary-change start-history-uuid (:start-input view) start-input)
                       (when-not active?
                         (boundary-change end-history-uuid (:end-input view) end-input))])
        session (cond-> {:start-history-uuid start-history-uuid}
                  (not active?) (assoc :end-history-uuid end-history-uuid))]
    (if (empty? changes)
      {:error :no-change}
      (let [{:keys [error updates]} (task-time/validate-edit {:history history
                                                              :now-ms now-ms
                                                              :session session
                                                              :changes changes})]
        (if error
          {:error error}
          {:session session :changes updates})))))
