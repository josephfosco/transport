;    Copyright (C) 2013-2014  Joseph Fosco. All Rights Reserved
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.

(ns transport.schedule
  (:require
   [transport.debug :refer [debug-transport debug-run1]]
   [transport.util.count-vector :refer [count-vector]]
   [transport.util.utils :refer :all]
   ))

(import '(java.util Date TimerTask Timer))
(declare check-events)

(def lateness (agent 0))      ; num of milliseconds most recent event was late
(def max-lateness (atom 0))   ; max num of milliseconds an event was late since starting scheduling
(def lateness-vector (count-vector)) ; vector to count lateness by amount
(def scheduler-running? (atom true)) ; If false, scheduler is paused and will not watch event-queue when it is empty
(def zero-time (atom nil))

(defn print-lateness
  []
  (println "schedule.clj - time: " (System/currentTimeMillis))
  (println "schedule.clj - lateness: " @lateness)
  (println "schedule.clj - max-lateness: " @max-lateness)
  (println "schedule.clj - lateness-vector: " ((lateness-vector :get)))
  )


(defn set-lateness
  "Used to set the lateness agent to new-val
   also sets max-lateness if appropriate

   agent-val - current agent value
   new-val value to set agent to"
  [agent-val new-val]
  (if (> new-val @max-lateness)
    (do
      (reset! max-lateness new-val)
      (print-msg "set-lateness" "****** new max-lateness: " @max-lateness)))
  ((lateness-vector :inc) new-val)
  new-val
  )

(defn init-lateness
  []
  ((lateness-vector :init) [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])
  ((lateness-vector :set-eval) (fn [num]
                                 (cond
                                  (< num 100) (int (/ num 10))
                                  (< num 1000) (+ (dec (int (/ num 100))) 10)
                                  (< num 2000) 19
                                  (< num 3000) 20
                                  (< num 4000) 21
                                  :else 22) ))
  )

(defn reset-lateness
  []
  (send-off lateness set-lateness 0)
  (await lateness)
  (reset! max-lateness 0)
  ((lateness-vector :init) [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])
  )

(defn reset-scheduler
  []
  (reset-lateness)
  (reset! zero-time nil)
  )

(defn event-queue-sort-fn
  "Sort function for the event que.
   First Compares the time of the events.
   If the times are equal, then compares the ids.
   If times and IDs are equal then the events are equal "
  [event1 event2]
  (cond
   (> (first event1) (first event2)) 1
   (< (first event1) (first event2)) -1
   (> (second event1) (second event2)) 1
   (< (second event1) (second event2)) -1
   :else 0))

(defn inc-event-counter
  [cur-counter-val]
  (inc cur-counter-val))

(let [event-queue (agent (sorted-set-by event-queue-sort-fn))
      the-timer (Timer.)    ; create a new Timer object
      next-TimerTask (atom nil)
      event-counter (atom (long 0)) ; a unique id attached to each event
      ]

  (defn print-event-queue
    []
    (print @event-queue)
    (println))

  (defn get-queue
    "Utility function to return the event-queue"
    []
    event-queue)

  (defn shutdown-event-queue
    "Shuts down the event-queue agent"
    []
    (shutdown-agents))

  (defn get-next-event-func
    []
    (nth (first @event-queue) 2))

  (defn get-next-event-data
    []
    (nth (first @event-queue) 3))

  (defn get-next-sched-event-time
    "Returns the time the next event to be executed is scheduled for.
      If the next event is scheduled for 0 then return the variable zero-time.
      If zero-time is nil set zero-time to current system time.
      zero-time is nil only for the very first event coming through"
    []
    (if (> (count @event-queue) 0)
      (let [next-queue-time (first (first @event-queue))]
        (if (> next-queue-time 0)
          next-queue-time
          (do
            (if (nil? @zero-time) (reset! zero-time (System/currentTimeMillis)))
            @zero-time
            )
          ))
      nil))

  (defn cancel-timerTask
    []
    (println "cancel-timerTask")
    (if (not (nil? @next-TimerTask))
      (.cancel @next-TimerTask))
    (println "timerTask canceled: " next-TimerTask))

  (defn cancel-timer
    []
    (.cancel the-timer)
    (reset-lateness)
    (debug-run1 (println "Timer canceled")))

  (defn remove-all-events
    [cur-queue]
    (sorted-set-by event-queue-sort-fn)
    )

  (defn remove-all-sched-events
    []
    (cancel-timerTask)
    (send event-queue remove-all-events)
    (await event-queue))

  (defn remove-first
    "Removes the first (head) event from the event-queue."
    [cur-queue]
    (disj cur-queue (first cur-queue)))

  (defn clear-scheduler
    "Clear scheduled events and reset lateness
     Does not stop or start scheduler"
    []
    (remove-all-sched-events)
    (reset-lateness)
    (reset! zero-time nil)
    )

  (defn sched-timer
    [sound-event]
    (debug-run1 (println "sched-timer"))
    (reset! next-TimerTask (proxy [TimerTask] [] (run [] (check-events))))
    (.schedule the-timer
               @next-TimerTask
               (Date. (long (first sound-event)))))

  (defn remove-watch-queue
    []
    (remove-watch event-queue :sound-start-scheduling)
    (println "Not Watching Queue"))

  (defn start-scheduling-events
    [key identity old new]
    (debug-run1 (println "STARTING SCHEDULING:" "new: " new))
    (remove-watch event-queue :sound-start-scheduling)
    (if (= (count  old) 0)
      (do
        (debug-run1
         (println "schedule current time: " (System/currentTimeMillis) " schedule time: " (first (first new))))
        (sched-timer (first new))
        (debug-run1 (println "next-TimerTask: " next-TimerTask))
        )
      (println "event-queue NOT nil")))

  (defn watch-queue
    []
    (if @scheduler-running?
      (do
        (add-watch event-queue :sound-start-scheduling start-scheduling-events)
        (println "Watching Queue")
        true)    ; return true if watch is added
      (do
        (println "Queue Watch NOT Added")
        false)))    ; return false if watch is not added

  (defn stop-scheduler
    []
    "Stops the scheduler from adding any events to be played.
     Also,stops the scheduler from watching for new events when
     the scheduler is restarted.
     To get the scheduler going again call restart-scheduler
     then schedule events. Events can be scheduled with init-player."
    (reset! scheduler-running? false))

  (defn restart-scheduler
    []
    (reset! scheduler-running? true))

  (defn check-events
    []
    (debug-run1 (println "1 check-events - current time: " (System/currentTimeMillis)))
    (debug-run1 (println "2 check-events - count: " (count @event-queue)))
    (debug-run1 (println "3 check-events - event-queue: " @event-queue))

    (while  (and (not= (count @event-queue) 0)
                 (<= (get-next-sched-event-time) (System/currentTimeMillis)))
      (do
        ;; first execute the scheduled function
        (if (coll? (get-next-event-data))
          (let [fnc (get-next-event-func)
                data (get-next-event-data)
                time (get-next-sched-event-time)
                ]
            (apply fnc `(~@data ~time))
            )
          ((get-next-event-func)
           (get-next-event-data)
           (get-next-sched-event-time))
          )
        ;; save lateness
        (send-off lateness set-lateness (- (System/currentTimeMillis) (get-next-sched-event-time)))
        (send event-queue remove-first)
        (await event-queue)
        (debug-run1 (println "4 check-events: " (count @event-queue)))))
    (if  (= (count @event-queue) 0)
      (do
        (println "Attempting to add Queue Watch")
        (watch-queue)
        )
      (sched-timer (first @event-queue))
      )
    (debug-run1 (println "5 END check-events END")))

  (defn start-scheduler
    "Starts the music event scheduler.
     If there are events in the queue it starts checking the queue.
     If there are no events in the queue it starts watching the queue."
    []
    (if (> (count @event-queue) 0)
      (check-events)
      (watch-queue)))

  (defn sched-event
    "Add an event to the event queue.
     If keyword :time is specified, event will be scheduled for that time (from System/currentTimeMillis)
       else event will be specified to occur event delay-millis from now.

     event-delay - is the number of milliseconds from now that this event will occur
     event-func - the function to call after event-delay has elapsed
     event-data - is the information that will be passed to the function that will
     be called when this event is executed.

     keyword :time - the time to fire to run this event. :time takes precedence over event-delay"
    [event-delay event-func event-data & {:keys [time]
                                          :or {time 0}}]
    (debug-run1 (println "1 sched-event - num items in event-queue:  " (count @event-queue)))
    (let [new-event-time (cond (> time 0) time
                               (> event-delay 0) (+ (System/currentTimeMillis) event-delay)
                               :else 0)
          new-event (list new-event-time (swap! event-counter inc-event-counter) event-func event-data)
          ;; if there are events in event-queue, and
          ;; this new event will be placed first in the event-queue
          ;; cancel the current timer and set sched-timer-flag to true
          ;; else sched-timer-fl is set to false
          sched-timer-fl (if (and (> (count @event-queue) 0)
                                  (not= new-event-time 0)
                                  (< new-event-time (get-next-sched-event-time))
                                  (> new-event-time (System/currentTimeMillis))
                                  )
                           (do
                             (cancel-timerTask)
                             true )
                           false)]
      (debug-run1 (println "2-EVENT-TIME: " new-event-time))
      (debug-run1 (println "3 sched-timer-fl: " sched-timer-fl))
      (comment
        (if @scheduler-running? ; only sched event if scheduler not paused (placed here for clarity)
          (if (and (> new-event-time 0) (< new-event-time (System/currentTimeMillis)))
            (do
              (if (coll? event-data)
                (apply event-func `(~@event-data ~new-event-time))
                (event-func event-data new-event-time))
              (send-off lateness set-lateness (- (System/currentTimeMillis) (get-next-sched-event-time)))
              (print-msg "sched-event" "execute immediately - not scheduled")
              )
            (do
              (send event-queue conj new-event )
              (await event-queue)
              (debug-run1 (println "4 Sent Event"))
              (if sched-timer-fl
                (sched-timer new-event))
              true))    ; return true if event was scheduled
          false)
        ) ; return false if event was not scheduled
      (if @scheduler-running? ; only sched event if scheduler not paused (placed here for clarity)
        (do
          (send event-queue conj new-event )
          (await event-queue)
          (debug-run1 (println "4 Sent Event"))
          (if sched-timer-fl
            (sched-timer new-event))
          true)    ; return true if event was scheduled
      false) ; return false if event was not scheduled
      ))
)
