(ns frontend.components.block.task-time-editor
  "Editable list of the tracked sessions derived from a task's status history."
  (:require [frontend.components.block.task-time-model :as model]
            [frontend.context.i18n :refer [t]]
            [frontend.handler.task-time :as task-time-handler]
            [frontend.util.clock :as clock]
            [io.factorhouse.hsx.core :as hsx]
            [lambdaisland.glogi :as log]
            [logseq.shui.hooks :as hooks]
            [logseq.shui.ui :as shui]
            [promesa.core :as p]))

(defn- duration-text
  [seconds]
  (or (not-empty (clock/seconds->days:hours:minutes:seconds seconds)) "0s"))

(defn- error-message
  [error]
  (case error
    (:session-not-found :boundary-not-in-session :history-not-found :history-not-in-task)
    (t :block.task-time/error-stale)
    :invalid-timestamp (t :block.task-time/error-invalid-timestamp)
    :start-not-before-end (t :block.task-time/error-start-not-before-end)
    :crosses-neighbor (t :block.task-time/error-crosses-neighbor)
    :start-in-future (t :block.task-time/error-start-in-future)
    :no-change (t :block.task-time/error-no-change)
    :save-failed (t :block.task-time/error-save-failed)))

(defn- time-field
  [{:keys [id label value on-change input-ref]}]
  [:div.flex.flex-col.gap-1
   [:label.text-xs {:for id} label]
   (shui/input
    {:id id
     :ref input-ref
     :type "datetime-local"
     :step 1
     :value value
     :on-change #(on-change (.. % -target -value))})])

(hsx/defc session-row
  [{:keys [task-uuid history session-view]}]
  (let [[editing? set-editing!] (hooks/use-state false)
        [start-input set-start-input!] (hooks/use-state (:start-input session-view))
        [end-input set-end-input!] (hooks/use-state (:end-input session-view))
        [error set-error!] (hooks/use-state nil)
        [saving? set-saving!] (hooks/use-state false)
        *edit-button (hooks/use-ref nil)
        *start-field (hooks/use-ref nil)
        *was-editing? (hooks/use-ref false)
        id-prefix (str "task-time-" (:key session-view))
        {:keys [active? start-text end-text seconds]} session-view
        close-editing! (fn []
                         (set-error! nil)
                         (set-editing! false))
        save! (fn []
                (let [{:keys [session changes error]}
                      (model/build-edit {:history history
                                         :now-ms (js/Date.now)
                                         :session-view session-view
                                         :start-input start-input
                                         :end-input end-input})]
                  (if error
                    (set-error! error)
                    (do
                      (set-saving! true)
                      (-> (task-time-handler/<edit-task-time! task-uuid session changes)
                          (p/then (fn [{worker-error :error}]
                                    (set-saving! false)
                                    (if worker-error
                                      (set-error! worker-error)
                                      (close-editing!))))
                          (p/catch (fn [e]
                                     (log/error :task-time/save-failed e)
                                     (set-saving! false)
                                     (set-error! :save-failed))))))))]
    (hooks/use-effect!
     (fn []
       (if editing?
         (some-> (hooks/deref *start-field) .focus)
         (when (hooks/deref *was-editing?)
           (some-> (hooks/deref *edit-button) .focus)))
       (hooks/set-ref! *was-editing? editing?)
       nil)
     [editing?])
    [:li.flex.flex-col.gap-2.py-1
     (if editing?
       [:form.flex.flex-col.gap-2
        {:on-submit (fn [e]
                      (.preventDefault e)
                      (save!))}
        [:div.flex.flex-row.gap-3.items-end.flex-wrap
         (time-field {:id (str id-prefix "-start")
                      :label (t :block.task-time/start)
                      :value start-input
                      :on-change set-start-input!
                      :input-ref *start-field})
         (if active?
           [:div.flex.flex-col.gap-1
            [:span.text-xs (t :block.task-time/end)]
            [:span.text-sm.py-1 (t :block.task-time/now)]]
           (time-field {:id (str id-prefix "-end")
                        :label (t :block.task-time/end)
                        :value end-input
                        :on-change set-end-input!}))]
        (when error
          [:div.text-sm.text-red-rx-09 {:role "alert"} (error-message error)])
        [:div.flex.flex-row.gap-2
         (shui/button {:type "submit" :size :sm :disabled saving?}
                      (t :block.task-time/save))
         (shui/button {:type "button" :size :sm :variant :ghost
                       :disabled saving?
                       :on-click close-editing!}
                      (t :block.task-time/cancel))]]
       [:div.flex.flex-row.gap-2.items-center.justify-between
        [:div.flex.flex-col
         [:div.text-sm
          [:span start-text]
          " – "
          [:span (if active? (t :block.task-time/now) end-text)]]
         [:div.text-xs.text-muted-foreground (duration-text seconds)]]
        (shui/button {:variant :ghost
                      :size :sm
                      :ref *edit-button
                      :aria-label (t :block.task-time/edit-session-label start-text)
                      :on-click (fn []
                                  (set-start-input! (:start-input session-view))
                                  (set-end-input! (:end-input session-view))
                                  (set-editing! true))}
                     (t :block.task-time/edit))])]))

(hsx/defc task-time-sessions-cp
  [{:keys [task-uuid history sessions]}]
  (when (seq sessions)
    [:div.flex.flex-col.gap-1.mb-3
     [:div.font-medium (t :block.task-time/sessions)]
     [:ul.flex.flex-col.divide-y.list-none.m-0.p-0
      (for [session-view (map model/session-view sessions)]
        ^{:key (:key session-view)}
        (session-row {:task-uuid task-uuid
                      :history history
                      :session-view session-view}))]]))
