(ns frontend.common.task-time-test
  (:require [cljs.test :refer [deftest is testing]]
            [frontend.common.task-time :as task-time]))

(def ^:private todo :logseq.property/status.todo)
(def ^:private doing :logseq.property/status.doing)
(def ^:private done :logseq.property/status.done)
(def ^:private canceled :logseq.property/status.canceled)
(def ^:private backlog :logseq.property/status.backlog)

(defn- history
  "Build ordered history from [created-at status-ident] pairs; uuids are the
  1-based position so assertions stay readable."
  [& entries]
  (vec (map-indexed (fn [idx [created-at status-ident]]
                      {:history-uuid (keyword (str "h" (inc idx)))
                       :created-at created-at
                       :status-ident status-ident})
                    entries)))

(deftest derive-time-test
  (testing "no history or a single event is untracked"
    (is (= {:sessions [] :seconds 0} (task-time/derive-time [] 9000)))
    (is (= {:sessions [] :seconds 0}
           (task-time/derive-time (history [1000 doing]) 9000))))
  (testing "doing -> done is one completed session"
    (is (= {:sessions [{:start-history-uuid :h1
                        :end-history-uuid :h2
                        :start-ms 1000
                        :end-ms 4000
                        :seconds 3
                        :active? false}]
            :seconds 3}
           (task-time/derive-time (history [1000 doing] [4000 done]) 9000))))
  (testing "multiple doing cycles stay separate sessions and sum"
    (let [{:keys [sessions seconds]}
          (task-time/derive-time
           (history [0 todo] [1000 doing] [3000 todo] [5000 doing] [9000 done])
           99999)]
      (is (= [[:h2 :h3 2] [:h4 :h5 4]]
             (map (juxt :start-history-uuid :end-history-uuid :seconds) sessions)))
      (is (= 6 seconds))))
  (testing "direct todo -> done counts"
    (let [{:keys [sessions seconds]}
          (task-time/derive-time (history [1000 todo] [6000 done]) 99999)]
      (is (= [[:h1 :h2 5]] (map (juxt :start-history-uuid :end-history-uuid :seconds) sessions)))
      (is (= 5 seconds))))
  (testing "backlog/canceled/done -> done does not count"
    (doseq [status [backlog canceled done]]
      (is (= 0 (:seconds (task-time/derive-time (history [1000 status] [6000 done]) 99999))))))
  (testing "custom statuses (nil ident) count into done but not into other states"
    (is (= 5 (:seconds (task-time/derive-time (history [1000 nil] [6000 done]) 99999))))
    (is (= 0 (:seconds (task-time/derive-time (history [1000 nil] [6000 todo]) 99999)))))
  (testing "an active doing session has no end and uses the explicit clock"
    (let [{:keys [sessions seconds]}
          (task-time/derive-time (history [0 todo] [1000 doing]) 8500)]
      (is (= [{:start-history-uuid :h2
               :start-ms 1000
               :seconds 7
               :active? true}]
             sessions))
      (is (= 7 seconds))))
  (testing "completed sessions plus an active one"
    (let [{:keys [sessions seconds]}
          (task-time/derive-time
           (history [0 todo] [1000 doing] [3000 todo] [5000 doing]) 8000)]
      (is (= [false true] (map :active? sessions)))
      (is (= 5 seconds))))
  (testing "total is floored once from summed milliseconds"
    (let [{:keys [sessions seconds]}
          (task-time/derive-time
           (history [0 todo] [0 doing] [900 todo] [1000 doing] [1900 todo]) 99999)]
      (is (= [0 0] (map :seconds sessions)))
      (is (= 1 seconds)))))

(deftest validate-edit-test
  (let [h (history [0 todo] [1000 doing] [5000 done] [7000 doing] [9000 todo])
        validate (fn [session changes]
                   (task-time/validate-edit {:history h
                                             :now-ms 99999
                                             :session session
                                             :changes changes}))
        first-session {:start-history-uuid :h2 :end-history-uuid :h3}]
    (testing "start-only, end-only and two-boundary edits"
      (is (= {:updates [{:history-uuid :h2 :created-at 500}]}
             (validate first-session [{:history-uuid :h2 :created-at 500}])))
      (is (= {:updates [{:history-uuid :h3 :created-at 6000}]}
             (validate first-session [{:history-uuid :h3 :created-at 6000}])))
      (is (= {:updates [{:history-uuid :h2 :created-at 2000}
                        {:history-uuid :h3 :created-at 3000}]}
             (validate first-session [{:history-uuid :h2 :created-at 2000}
                                      {:history-uuid :h3 :created-at 3000}]))))
    (testing "a boundary may move past its old neighbour only up to the next event"
      (is (= {:error :crosses-neighbor}
             (validate first-session [{:history-uuid :h3 :created-at 7000}])))
      (is (= {:error :crosses-neighbor}
             (validate {:start-history-uuid :h4 :end-history-uuid :h5}
                       [{:history-uuid :h4 :created-at 5000}]))
          "only the first event is shifted, never a previous session's boundary"))
    (testing "moving the start before the first non-Doing event shifts that event"
      (is (= {:error :invalid-timestamp}
             (validate first-session [{:history-uuid :h2 :created-at 0}]))
          "the shifted first event would land before the epoch")
      (let [h' (history [10000 todo] [11000 doing] [15000 done])
            result (task-time/validate-edit
                    {:history h'
                     :now-ms 99999
                     :session {:start-history-uuid :h2 :end-history-uuid :h3}
                     :changes [{:history-uuid :h2 :created-at 4000}]})]
        (is (= {:updates [{:history-uuid :h2 :created-at 4000}]
                :shifted [{:history-uuid :h1 :created-at 3000}]}
               result))
        (is (= {:updates [{:history-uuid :h2 :created-at 10000}]
                :shifted [{:history-uuid :h1 :created-at 9000}]}
               (task-time/validate-edit
                {:history h'
                 :now-ms 99999
                 :session {:start-history-uuid :h2 :end-history-uuid :h3}
                 :changes [{:history-uuid :h2 :created-at 10000}]}))
            "a start equal to the first event also shifts it")
        (is (= {:updates [{:history-uuid :h2 :created-at 10500}]}
               (task-time/validate-edit
                {:history h'
                 :now-ms 99999
                 :session {:start-history-uuid :h2 :end-history-uuid :h3}
                 :changes [{:history-uuid :h2 :created-at 10500}]}))
            "a start after the first event shifts nothing")))
    (testing "start must stay before end"
      (is (= {:error :start-not-before-end}
             (validate first-session [{:history-uuid :h2 :created-at 5000}])))
      (is (= {:error :start-not-before-end}
             (validate first-session [{:history-uuid :h2 :created-at 4000}
                                      {:history-uuid :h3 :created-at 4000}])))
      (is (= {:error :start-not-before-end}
             (validate first-session [{:history-uuid :h3 :created-at 800}]))))
    (testing "unchanged timestamps are not an edit"
      (is (= {:error :no-change}
             (validate first-session [{:history-uuid :h2 :created-at 1000}]))))
    (testing "invalid timestamps"
      (doseq [value [-1 1.5 "1000" nil js/NaN]]
        (is (= {:error :invalid-timestamp}
               (validate first-session [{:history-uuid :h2 :created-at value}]))
            (pr-str value))))
    (testing "stale or foreign sessions and boundaries"
      (is (= {:error :session-not-found}
             (validate {:start-history-uuid :h2 :end-history-uuid :h4}
                       [{:history-uuid :h2 :created-at 500}])))
      (is (= {:error :session-not-found}
             (validate {:start-history-uuid :missing :end-history-uuid :h3}
                       [{:history-uuid :h2 :created-at 500}])))
      (is (= {:error :boundary-not-in-session}
             (validate first-session [{:history-uuid :h4 :created-at 6000}])))
      (is (= {:error :boundary-not-in-session}
             (validate first-session [{:history-uuid :missing :created-at 6000}]))))
    (testing "the other session is untouched by editing one"
      (let [{:keys [updates]} (validate {:start-history-uuid :h4 :end-history-uuid :h5}
                                        [{:history-uuid :h4 :created-at 6000}])]
        (is (= [{:history-uuid :h4 :created-at 6000}] updates))))
    (testing "malformed requests throw"
      (is (thrown? js/Error (validate first-session [])))
      (is (thrown? js/Error (validate first-session [{:history-uuid :h2 :created-at 500}
                                                     {:history-uuid :h2 :created-at 600}]))))))

(deftest validate-edit-with-empty-history-test
  (is (= {:error :session-not-found}
         (task-time/validate-edit {:history []
                                   :now-ms 5000
                                   :session {:start-history-uuid (random-uuid)}
                                   :changes [{:history-uuid (random-uuid) :created-at 500}]}))
      "With no history there is no derived session to edit."))

(deftest validate-active-session-edit-test
  (let [h (history [0 todo] [1000 doing])
        validate (fn [changes]
                   (task-time/validate-edit {:history h
                                             :now-ms 8000
                                             :session {:start-history-uuid :h2}
                                             :changes changes}))]
    (is (= {:updates [{:history-uuid :h2 :created-at 500}]}
           (validate [{:history-uuid :h2 :created-at 500}])))
    (is (= {:error :invalid-timestamp}
           (validate [{:history-uuid :h2 :created-at 0}])))
    (is (= {:error :start-in-future}
           (validate [{:history-uuid :h2 :created-at 8000}])))
    (is (= {:error :boundary-not-in-session}
           (validate [{:history-uuid :h1 :created-at 10}])))))
