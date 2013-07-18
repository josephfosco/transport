;    Copyright (C) 2013  Joseph Fosco. All Rights Reserved
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
  (:use [ transport.debug :only [debug-transport debug-run1]]))

(import '(java.util Date TimerTask Timer))
(declare check-events)

(def lateness 0)      ; num of milliseconds most recent event was late
(def max-lateness 0)  ; max num of milliseconds an event was late since starting scheduling
(def sched-events-fl true) ; do not sched events if false
(def queue-watch-fl true)    ; If false, scheduler will not watch event-queue when it is empty

(defn print-lateness []
  (println "lateness: " lateness)
  (println "max-lateness: " max-lateness)
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

(defn inc-event-counter [cur-counter-val]
  (inc cur-counter-val))

(let [event-queue (agent (sorted-set-by event-queue-sort-fn))
      the-timer (Timer.)    ; create a new Timer object
      next-TimerTask (atom nil)
      event-counter (atom (long 0)) ; a unique id attached to each event
      ]

  (defn get-queue
    "Utility function to return the event-queue"
    []
    event-queue)

  (defn next-sched-event-time
    "Returns the time the next event to be executed is scheduled for"
    []
    (if (> (count @event-queue) 0)
      (first (first @event-queue))
      nil))

  (defn cancel-timerTask []
    (println "cancel-timerTask")
    (if (not (nil? @next-TimerTask))
      (.cancel @next-TimerTask))
    (println "timerTask canceled: " next-TimerTask))

  (defn cancel-timer []
    (.cancel the-timer)
    (def lateness 0)
    (def max-lateness 0)
    (debug-run1 (println "Timer canceled")))

  (defn remove-all-events [cur-queue]
    (if (= (count cur-queue) 1)
      (disj cur-queue (first cur-queue))
      (recur (disj cur-queue (first cur-queue)))))

  (defn remove-all-sched-events []
    (cancel-timerTask)
    (send event-queue remove-all-events)
    (await event-queue))

  (defn remove-first
    "Removes the first (head) event from the event-queue."
    [cur-queue]
    (disj cur-queue (first cur-queue)))

  (defn sched-timer [sound-event]
    (debug-run1 (println "sched-timer"))
    (reset! next-TimerTask (proxy [TimerTask] [] (run [] (check-events))))
    (.schedule the-timer
               @next-TimerTask
               (Date. (long (first sound-event)))))

  (defn remove-watch-queue []
    (remove-watch event-queue :sound-start-scheduling)
    (println "Not Watching Queue"))

  (defn start-scheduling-events [key identity old new]
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

  (defn watch-queue []
    (if queue-watch-fl
      (do
        (add-watch event-queue :sound-start-scheduling start-scheduling-events)
        (println "Watching Queue")
        true)    ; return true if watch is added
      false))    ; return false if watch is not added

  (defn stop-scheduler []
    "Stops the scheduler from adding any events to be played.
     Also,stops the scheduler from watching for new events when
     the scheduler is restarted.
     To get the scheduler going again call restart-scheduler
     then schedule events. Events can be scheduled with init-player."
    (def queue-watch-fl false)
    (def sched-events-fl false))

  (defn restart-scheduler []
    (def queue-watch-fl true)
    (def sched-events-fl true))

  (defn next-event-data []
    (nth (first @event-queue) 2))

  (defn check-events []
    (debug-run1 (println "1 check-events - current time: " (System/currentTimeMillis)))
    (debug-run1 (println "2 check-events - count: " (count @event-queue)))
    (debug-run1 (println "3 check-events - event-wueue: " @event-queue))

    (while  (and (not= (count @event-queue) 0)
                 (<= (next-sched-event-time) (System/currentTimeMillis)))
      (do
        ((:function (next-event-data))
         (next-event-data)
         (if (> (next-sched-event-time) 0) (next-sched-event-time) (System/currentTimeMillis)))
        ; if event time is > 0 save lateness and, if necessary max-lateness
        (if (> (next-sched-event-time) 0)
          (do
            (def lateness (- (System/currentTimeMillis) (next-sched-event-time)))
            (if (> lateness max-lateness)
              (def max-lateness lateness)
              )))
        (send event-queue remove-first)
        (await event-queue)
        (debug-run1 (println "4 check-events: " (count @event-queue)))))
    (if  (= (count @event-queue) 0)
      (do
        (println "ADDING QUEUE WATCH")
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

  (defn sched-event [event-delay event-map]
    (debug-run1 (println "1 sched-event - num items in event-queue:  " (count @event-queue)))
    (let [new-event-time (if event-delay (+ (System/currentTimeMillis) event-delay) 0)
          new-event (list new-event-time (swap! event-counter inc-event-counter) event-map)
          ; if there are events in event-queue, and
          ; this new event will be placed first in the event-queue
          ; cancel the current timer and set sched-timer-flag to true
          ; else sched-timer-fl is set to false
          sched-timer-fl (if (and (> ( count @event-queue) 0)
                                  (< new-event-time (next-sched-event-time)))
                           (do
                             (cancel-timerTask)
                             true )
                           false)]
      (debug-run1 (println "2-EVENT-TIME: " new-event-time))
      (debug-run1 (println "3 sched-timer-fl: " sched-timer-fl))
      (if sched-events-fl  ; only sched event if fl is true (placed here for clarity)
        (do
          (send event-queue conj new-event )
          (await event-queue)
          (debug-run1 (println "4 Sent Event"))
          (if sched-timer-fl
            (sched-timer new-event))
          true)    ; return true if event was scheduled
        false)))   ; return false if event was not scheduled
)
