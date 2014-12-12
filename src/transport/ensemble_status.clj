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

(ns transport.ensemble-status
  (:require
   [transport.behavior :refer [get-behavior-action]]
   [transport.dur-info :refer [get-dur-millis]]
   [transport.melodychar :refer :all]
   [transport.melodyevent :refer [get-dur-info-for-event get-note-for-event]]
   [transport.messages :refer :all]
   [transport.message-processor :refer [register-listener send-message]]
   [transport.players :refer :all]
   [transport.random :refer [random-int]]
   [transport.schedule :refer [sched-event]]
   [transport.settings :refer :all]
   [transport.util.utils :refer :all])
  (:import (java.util Date TimerTask Timer))
   )

(def note-values-millis (atom '(0 0 0 0 0 0 0 0 0 0)))
;; player-keys, -mms, and -volumes are vectors of the last respective values
;;  for each player.
;;  player-id is index into vector.
(def player-keys (atom (apply vector (repeat @number-of-players (rand 12)))))
(def player-mms (atom (apply vector (repeat @number-of-players nil))))
(def player-volumes (atom (apply vector (repeat @number-of-players 0))))
(def player-continuities (atom (apply vector (repeat @number-of-players 0))))
(def player-densities (atom (apply vector (repeat @number-of-players 0))))

(def rest-prob-len (atom 0))
;; rest-prob is list of true for notes, false for rests
(def rest-prob (atom '()))
(def note-times (agent '()))

(def prev-ensemble-density (atom 0))
(def density-trend (atom INCREASING))

(defn update-note-times
  [cur-note-times new-value]
  (conj cur-note-times new-value)
  )

(defn- players-soft?
  "Returns true if the current volume of 90% of all players except
   exception-player-id is less than .25"
  [exception-player-id]
  (loop [rslt '() vols-to-check (assoc @player-volumes exception-player-id 0)]
    (cond (> (count rslt) (* @number-of-players 0.1)) false
          (empty? vols-to-check) true
          (>= (first vols-to-check) 0.25) (recur (conj rslt true) (rest vols-to-check))
          :else (recur rslt (rest vols-to-check))
          )
    )
  )

(defn- send-status-msgs
  [player player-last-melody player-id note-time]
  (if (and (> (get-volume-for-note player-last-melody) 0.85)
           (> (get-dur-millis-for-note player-last-melody) 3000)
           (players-soft? player-id)
           (not= (get-behavior-action (get-behavior player)) FOLLOW-PLAYER))
    (do
      (print-msg "send-status-msgs" "volume: "(get-volume-for-note player-last-melody))
      (print-msg "send-status-msgs" "dur-millis: " (get-dur-millis-for-note player-last-melody))
      (print-msg "send-status-msgs players-volumes:" @player-volumes)

      (send-message MSG-LOUD-INTERUPT-EVENT :player-id player-id :time note-time)
      (print-msg "send-status-msgs" "SENDING LOUD-INTERRUPT-EVENT MSG")
      )
    )
  )

(defn update-ensemble-new-segment
  [& {:keys [player note-time player-id]}]
  (if (not= (get-key player) (get player-keys player-id))
    (reset! player-keys (assoc @player-keys player-id (get-key player))) )
  (if (not= (get-mm player) (get player-mms player-id))
    (reset! player-mms (assoc @player-mms player-id (get-mm player))) )
  (if (not= (get-melody-char-density (get-melody-char player)) (get player-densities player-id))
    (reset! player-densities (assoc @player-densities player-id (get-melody-char-density (get-melody-char player)))))
  (if (not= (get-melody-char-continuity (get-melody-char player)) (get player-continuities player-id))
    (reset! player-continuities (assoc @player-continuities player-id (get-melody-char-continuity (get-melody-char player)))))
  )

(defn update-ensemble-status
  [& {:keys [player note-time]}]
  (let [last-melody (get-last-melody-event player)
        player-id (get-player-id player)
        ]
    ;; update this player's volume in player-volumes
    (reset! player-volumes (assoc @player-volumes player-id (get-volume-for-note last-melody)))
    ;; Track relevent ensemble-status info when player starts a new segment
    (update-ensemble-new-segment :player player :note-time note-time :player-id player-id)
    ;; if note (not rest) update note-values-millis with latest note rhythm value
    ;;   and rest-prob (with new note)
    ;; else just update rest-prob (with new rest)
    (if (not (nil? (get-note-for-event last-melody)))
      (do
        (let [dur-millis (get-dur-millis (get-dur-info-for-event last-melody))]
          (reset! note-values-millis (conj (butlast @note-values-millis) dur-millis))
          (send-off note-times update-note-times (list note-time dur-millis)))
        (reset! rest-prob (conj (butlast @rest-prob) true))
        )
      (do
        (reset! rest-prob (conj (butlast @rest-prob) false))
        )
      )
    (send-status-msgs player last-melody player-id note-time)
    )
  )

(defn get-average-note-dur-millis
  []
  (average @note-values-millis @number-of-players))

(defn get-average-volume
  []
  (average @player-volumes @number-of-players))

(defn get-rest-probability
  "Compute the percent of rests in rest-prob. Returns fraction or float."
  []
  (/ (count (filter #(= false %1) @rest-prob)) @rest-prob-len))

(defn get-ensemble-key-for-player
  "Select a key for player from keys currently playing in ensemble"
  [player]
  (let [rand-index (rand-int (dec @number-of-players)) ;; select a rand index into player-keys
        ]
    (if (>= rand-index (get-player-id player))   ;; return a key from player-keys but
      (get @player-keys (inc rand-index))        ;;  do not return key for player
      (get @player-keys rand-index))
    )
  )

(defn get-ensemble-mm
  "Returns the mm most used in the ensemble.
   If all mms are unique, returns one mm from the ensemble."
  []
  (print-msg "get-ensemble-mm:" @player-mms)
  (get-vector-max-frequency @player-mms)
  )

(defn get-ensemble-continuity
  []
  (print-msg "get-ensemble-continuity:" @player-continuities)
  (get-vector-max-frequency @player-continuities)
  )

(defn get-average-continuity
  []
  (average @player-continuities @number-of-players))

(defn get-ensemble-most-common-density
  []
  (print-msg "get-ensemble-most-common-density:   " @player-densities)
  (get-vector-max-frequency @player-densities)
  )

(defn get-average-density
  []
  (average @player-densities @number-of-players))

(defn get-note-dur-list
  [cur-note-times to-time]
  (for [note-info cur-note-times
        :let [note-time (first note-info)
              note-dur (second note-info)
              dur (if (> (+ note-time note-dur) to-time) (- to-time note-time) note-dur)]]
    dur)
  )

(defn get-ensemble-density-ratio
  "Returns ratio of time sound is present to total time, in a
   arbitrary amount of recent time"
  [& {:keys [cur-note-times]
      :or {cur-note-times @note-times}}]

  (let [cur-time (System/currentTimeMillis)
        first-note-time (first (last cur-note-times))
        total-note-time (apply + (get-note-dur-list cur-note-times cur-time))
        ]
    ;; first-note time will be nil if note-times is empty
    ;; this can happen after a loud-event-msg
    (if first-note-time
      (/ total-note-time (* (- cur-time first-note-time) @number-of-players))
      0)
    )
 )

(defn get-ensemble-density
  []
  (round-number (* 10 (get-ensemble-density-ratio)))
 )

(defn get-density-trend
  []
  @density-trend)

(defn remove-expired-times
  [cur-note-times]
  (let [earliest-time (- (System/currentTimeMillis) 2000)]
    ;; remove all entries in note-times that have ended more than 2 secs ago
    (for [note-time cur-note-times
          :let [end-time (+ (first note-time) (second note-time))
                rtn-entry (cond (and (< (first note-time) earliest-time) (> end-time earliest-time))
                                (list earliest-time (- end-time earliest-time))
                                (< end-time earliest-time)
                                nil
                                :else note-time
                                )
                ]

          :when (not (nil? rtn-entry))] rtn-entry))
  )

(defn check-activity
  []
;;  (print-msg "check-activity" "current-time: " (System/currentTimeMillis))
;;  (print-msg "check-activity" "note-times: " @note-times)
  (let [cur-ensemble-density (get-ensemble-density-ratio :cur-note-times @note-times)]

    (print-msg "check-activity" "prev-ensemble-density: " (float @prev-ensemble-density) " cur-ensemble-density: " (float cur-ensemble-density) " length note-times: " (count @note-times))

    (reset! density-trend (cond
                           (>= (- cur-ensemble-density @prev-ensemble-density) 0.05)
                           (do
                             (reset! prev-ensemble-density cur-ensemble-density)
                             INCREASING
                             )
                           (<= (- cur-ensemble-density @prev-ensemble-density) -0.05)
                           (do
                             (reset! prev-ensemble-density cur-ensemble-density)
                             DECREASING
                             )
                           :else STEADY
                           )
            )
    (print-msg "check-activity" " density-trend: " @density-trend)

    )
  )

(defn upd-density-trend
  [& args]
  (check-activity)
  (send-off note-times remove-expired-times)
  (sched-event 5000 transport.ensemble-status/upd-density-trend nil)
  )

(defn init-ensemble-status
  []
  (reset! note-values-millis '(0 0 0 0 0 0 0 0 0 0))

  (reset! player-keys (apply vector (repeat @number-of-players (rand 12))))
  (reset! player-mms (apply vector (repeat @number-of-players nil)))
  (reset! player-volumes (apply vector (repeat @number-of-players 0)))
  (reset! rest-prob-len (* @number-of-players 3))
  ;; initialize rest-prob
  (reset! rest-prob '())
  (dotimes [n @rest-prob-len]
    (if (< (rand) 0.8)
      (reset! rest-prob (conj @rest-prob true))
      (reset! rest-prob (conj @rest-prob false))
      ))

  (reset! player-continuities (apply vector (repeat @number-of-players 0)))
  (reset! player-densities (apply vector (repeat @number-of-players 0)))

  (reset! prev-ensemble-density 0)
  (reset! density-trend INCREASING)

  ;; update ensemble-status with each new note
  (register-listener
   MSG-PLAYER-NEW-NOTE
   transport.ensemble-status/update-ensemble-status
   {}
   )
  (sched-event 5000 transport.ensemble-status/upd-density-trend nil)
  )

(defn reset-ensemble-status
  []
  (init-ensemble-status)
  )
