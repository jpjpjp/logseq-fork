(ns frontend.components.block.task-time-model-test
  (:require [cljs.test :refer [deftest is testing]]
            [frontend.components.block.task-time-model :as model]
            [frontend.common.task-time :as task-time]))

(def ^:private todo :logseq.property/status.todo)
(def ^:private doing :logseq.property/status.doing)
(def ^:private done :logseq.property/status.done)

(defn- local-ms
  [& args]
  (.getTime (apply #(js/Date. %1 %2 %3 %4 %5 %6) args)))

(def ^:private t0 (local-ms 2026 5 10 9 0 0))
(def ^:private t1 (+ t0 3600000))
(def ^:private t2 (+ t0 7200000))

(def ^:private history
  [{:history-uuid :h1 :created-at (- t0 60000) :status-ident todo}
   {:history-uuid :h2 :created-at t0 :status-ident doing}
   {:history-uuid :h3 :created-at t1 :status-ident done}
   {:history-uuid :h4 :created-at t2 :status-ident doing}])

(def ^:private now-ms (+ t2 5000))

(def ^:private views
  (mapv model/session-view (:sessions (task-time/derive-time history now-ms))))

(deftest local-input-round-trip-test
  (testing "formatting and parsing are inverse in the local zone"
    (is (= "2026-06-10T09:00:00" (model/format-local-input t0)))
    (is (= "2026-06-10 09:00:00" (model/format-local-display t0)))
    (is (= t0 (model/parse-local-input "2026-06-10T09:00:00")))
    (is (= t0 (model/parse-local-input "2026-06-10T09:00")))
    (is (= (+ t0 7000) (model/parse-local-input "2026-06-10T09:00:07"))))
  (testing "ranges across midnight stay on the local calendar"
    (let [ms (local-ms 2026 5 10 23 59 59)]
      (is (= "2026-06-10T23:59:59" (model/format-local-input ms)))
      (is (= "2026-06-11T00:00:00" (model/format-local-input (+ ms 1000))))))
  (testing "malformed or impossible values are rejected"
    (doseq [value [nil "" "garbage" "2026-06-10" "2026-06-10 09:00:00" "2026-13-10T09:00:00"
                   "2026-02-30T09:00:00" "2026-06-10T25:00:00" "2026-06-10T09:61:00"]]
      (is (nil? (model/parse-local-input value)) (pr-str value))))
  (testing "a daylight-saving gap is rejected when the zone has one"
    (let [gap (js/Date. 2026 2 8 2 30 0)]
      (when (not= 2 (.getHours gap))
        (is (nil? (model/parse-local-input "2026-03-08T02:30:00")))))))

(deftest session-view-test
  (is (= [{:seconds 3600 :active? false
           :start-text "2026-06-10 09:00:00" :end-text "2026-06-10 10:00:00"
           :start-input "2026-06-10T09:00:00" :end-input "2026-06-10T10:00:00"}
          {:seconds 5 :active? true
           :start-text "2026-06-10 11:00:00" :end-text nil
           :start-input "2026-06-10T11:00:00" :end-input nil}]
         (mapv #(select-keys % [:seconds :active? :start-text :end-text :start-input :end-input])
               views)))
  (is (= [:h2 :h3] ((juxt :start-history-uuid :end-history-uuid) (first views))))
  (is (nil? (:end-history-uuid (second views)))))

(deftest build-edit-test
  (let [completed (first views)
        active (second views)
        build (fn [view start end]
                (model/build-edit {:history history :now-ms now-ms :session-view view
                                   :start-input start :end-input end}))]
    (testing "only changed boundaries are sent"
      (is (= {:session {:start-history-uuid :h2 :end-history-uuid :h3}
              :changes [{:history-uuid :h2 :created-at (- t0 1000)}]}
             (build completed "2026-06-10T08:59:59" "2026-06-10T10:00:00")))
      (is (= {:session {:start-history-uuid :h2 :end-history-uuid :h3}
              :changes [{:history-uuid :h3 :created-at (+ t1 1000)}]}
             (build completed "2026-06-10T09:00:00" "2026-06-10T10:00:01")))
      (is (= 2 (count (:changes (build completed "2026-06-10T09:30:00" "2026-06-10T10:30:00"))))))
    (testing "nothing changed"
      (is (= {:error :no-change}
             (build completed "2026-06-10T09:00:00" "2026-06-10T10:00:00"))))
    (testing "validation errors"
      (is (= {:error :invalid-timestamp}
             (build completed "" "2026-06-10T10:00:00")))
      (is (= {:error :start-not-before-end}
             (build completed "2026-06-10T10:00:00" "2026-06-10T10:00:00")))
      (is (= {:error :crosses-neighbor}
             (build completed "2026-06-10T09:00:00" "2026-06-10T11:30:00"))))
    (testing "an active session edits only its start and ignores the end input"
      (is (= {:session {:start-history-uuid :h4}
              :changes [{:history-uuid :h4 :created-at (+ t2 2000)}]}
             (build active "2026-06-10T11:00:02" "2030-01-01T00:00:00")))
      (is (= {:error :crosses-neighbor}
             (build active "2026-06-10T09:30:00" nil)))
      (is (= {:error :start-in-future}
             (build active "2026-06-10T11:00:06" nil))))))
