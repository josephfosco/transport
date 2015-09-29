;    Copyright (C) 2013-2015  Joseph Fosco. All Rights Reserved
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
   [overtone.live :refer [MIDI-RANGE]]
   [transport.behavior :refer [get-behavior-action]]
   [transport.constants :refer :all]
   [transport.dur-info :refer [get-dur-millis]]
   [transport.melodychar :refer :all]
   [transport.melodyevent :refer [get-dur-info-for-event get-note-for-event get-volume-for-event]]
   [transport.messages :refer :all]
   [transport.message-processor :refer [register-listener send-message]]
   [transport.players :refer :all]
   [transport.random :refer [random-int]]
   [transport.schedule :refer [sched-event]]
   [transport.settings :refer [ensemble-density-change-threshold ensemble-mm-change-threshold
                               ensemble-pitch-change-threshold ensemble-volume-change-threshold
                               min-mm max-mm min-volume number-of-players]]
   [transport.util.compare-prior-current :refer :all]
   [transport.util.count-vector :refer [count-vector]]
   [transport.util.track-trend :refer [track-trend]]
   [transport.util.utils :refer :all])
  (:import (java.util Date TimerTask Timer))
   )

(def INITIAL-NUM-VOLUME-TREND 10)
(def INITIAL-NUM-DENSITY-TREND 5)
(def INITIAL-NUM-PITCH-TREND 11)
(def trend-upd-millis 3000)
(def midi-mid (int (/ MIDI-HI 2)))

(def note-values-millis (atom '(0 0 0 0 0 0 0 0 0 0)))
;; player-keys, -mms, and -volumes are vectors of the last respective values
;;  for each player.
;;  player-id is index into vector.
(def player-keys (atom (apply vector (repeat @number-of-players (rand 12)))))
(def player-mms (atom (apply vector (repeat @number-of-players nil))))
(def mm-trend (compare-prior-current))
(def player-volumes (atom (apply vector (repeat @number-of-players 0))))
(def volume-trend (track-trend))
(def player-densities (atom (apply vector (repeat @number-of-players 0))))
(def density-trend (track-trend))
(def player-note-durs (atom (apply vector (repeat @number-of-players 0))))
(def player-notes (atom (apply vector (repeat @number-of-players midi-mid))))
(def pitch-trend (track-trend))

(def note-times (agent '()))

(defn update-note-times
  [cur-note-times new-value]
  (conj cur-note-times new-value)
  )

(defn- players-soft?
  "Returns true if the current volume of 95% of all players except
   exception-player-id is less than .3"
  [exception-player-id]
  (loop [rslt '() vols-to-check (assoc @player-volumes exception-player-id 0)]
    (cond (> (count rslt) (* @number-of-players 0.05)) false
          (empty? vols-to-check) true
          (>= (first vols-to-check) (+ @min-volume 0.1)) (recur (conj rslt true) (rest vols-to-check))
          :else (recur rslt (rest vols-to-check))
          )
    )
  )

(defn- send-status-msgs
  [player player-last-melody player-id note-time]
  (if (and (> (get-volume-for-event player-last-melody) 0.85)
           (> (get-dur-millis (get-dur-info-for-event player-last-melody)) 3000)
           (players-soft? player-id)
           (not= (get-behavior-action (get-behavior player)) FOLLOW-PLAYER)
           (> (count (filter #(not= 0 %1) @player-volumes)) (int (/ @number-of-players 2))))
    (do
      (print-msg "send-status-msgs" "volume: "(get-volume-for-event player-last-melody))
      (print-msg "send-status-msgs" "dur-millis: " (get-dur-millis (get-dur-info-for-event player-last-melody)))
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
  (let [melody-char (get-melody-char player)]
    (if (not= (get-melody-char-note-durs melody-char) (get player-note-durs player-id))
      (reset! player-note-durs (assoc @player-note-durs player-id
                                      (get-melody-char-note-durs melody-char))))
    (if (not= (get-melody-char-density melody-char) (get @player-densities player-id))
      (reset! player-densities (assoc @player-densities player-id (get-melody-char-density melody-char))))
    )
  )

(defn update-ensemble-status
  [& {:keys [player note-time]}]
  (let [last-melody (get-last-melody-event player)
        player-id (get-player-id player)
        ]
    ;; update this player's volume in player-volumes
    (reset! player-volumes (assoc @player-volumes player-id (get-volume-for-event last-melody)))
    ;; update this player's pitch in player-notes
    (reset! player-notes (assoc @player-notes player-id (get-note-for-event last-melody)))
    ;; Track relevent ensemble-status info when player starts a new segment
    (update-ensemble-new-segment :player player :note-time note-time :player-id player-id)
    ;; if note (not rest) update note-values-millis with latest note rhythm value
    (if (not (nil? (get-note-for-event last-melody)))
      (do
        (let [dur-millis (get-dur-millis (get-dur-info-for-event last-melody))]
          (reset! note-values-millis (conj (butlast @note-values-millis) dur-millis))
          (send-off note-times update-note-times (list note-time dur-millis)))
        )
      )
    (send-status-msgs player last-melody player-id note-time)
    )
  )

(defn get-average-note-dur-millis
  []
  (average @note-values-millis :list-length @number-of-players))

(defn get-average-volume
  []
  (average @player-volumes :list-length @number-of-players))

(defn get-average-playing-volume
  []
  "Returns the average volume of all players playing a note"
  (let [vols (for [vol @player-volumes :when (> vol 0)] vol)]
    (if (not= (first vols) nil)
      (average vols)
      0
      )))

(defn get-ensemble-trend-volume
  []
  ((volume-trend :trend))
  )

(defn get-volume-trend-diff
  []
  ((volume-trend :trend-diff))
  )

(defn get-average-mm
  []
  (round-number (average @player-mms :list-length @number-of-players)))

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
  ;; (print-msg "get-ensemble-mm:" @player-mms)
  (get-vector-max-frequency @player-mms)
  )

(defn get-ensemble-trend-mm
  "Returns mm most that is the average ensemble-mm +/- the ensemble mm trend"
  []
;;  (print-msg "get-ensemble-trend-mm:" @player-mms)
;;  (print-msg "get-ensemble-trend-mm:" "mm-trend: " ((mm-trend :trend-amount)) " average: " (get-average-mm) " ensemble-mm " (get-ensemble-mm))
  (let [ensemble-mm (+ (get-ensemble-mm) ((mm-trend :trend-amount)))]
    (cond (> @min-mm ensemble-mm @max-mm) ensemble-mm
          (< ensemble-mm @min-mm) @min-mm
          :else @max-mm
     ))
  )

(defn get-ensemble-max-freq-density
  []
  (get-vector-max-frequency @player-densities)
  )

(defn get-average-density
  []
  (average @player-densities :list-length @number-of-players))

(defn get-pitch-trend
  []
  ((pitch-trend :trend))
  )

(defn get-pitch-trend-diff
  []
  ((pitch-trend :trend-diff))
  )

(defn get-ensemble-average-pitch
  []
  (let [pitches (for [p @player-notes :when (not (nil? p))] p)]
    (if (first pitches)
      (average pitches)
      nil)
    )
  )

(defn get-ensemble-most-common-note-durs
  []
  (get-vector-max-frequency @player-note-durs)
  )

(defn get-average-note-durs
  []
  (average @player-note-durs :list-length @number-of-players))

(defn get-note-dur-list
  " Returns a list of note-durs starting at the earliest dur in curnote-times
    up til to-time

    cur-note-times - a list of lists of note-start-times and durations
    to-time - the latest time (in millis) to ruturn a duration for"
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
  (Math/round (float (* 10 (get-ensemble-density-ratio))))
 )

(defn get-density-trend
  []
  ((density-trend :trend))
  )

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

(defn print-density
  []
  (println "ensemble_status.clj - time: " (System/currentTimeMillis))
  (println "ensemble_status.clj - density-trend: " (get-density-trend))
  )

(defn upd-ensemble-trends
  [& args]
  (print-msg "upd-ensemble-trends" "cur-ensemble-density: "
             (float (get-ensemble-density-ratio :cur-note-times @note-times)) " density-trend: " (get-density-trend)
             " length note-times: " (count @note-times))
  (send-off note-times remove-expired-times)
  ((mm-trend :new-value) (round-number (get-average-mm)))
  ((volume-trend :new-value-to-track) (get-average-playing-volume))
  ((density-trend :new-value-to-track) (get-ensemble-density-ratio))
  ((pitch-trend :new-value-to-track) (get-ensemble-average-pitch))
  (sched-event trend-upd-millis transport.ensemble-status/upd-ensemble-trends nil)
  )

(defn init-ensemble-status
  []
  (reset! note-values-millis '(0 0 0 0 0 0 0 0 0 0))

  (reset! player-keys (apply vector (repeat @number-of-players (rand 12))))
  (reset! player-mms (apply vector (repeat @number-of-players nil)))
  ((mm-trend :init) @ensemble-mm-change-threshold)
  (reset! player-volumes (apply vector (repeat @number-of-players 0)))
  ((volume-trend :init) (map (fn [x] 0) (range INITIAL-NUM-VOLUME-TREND)) @ensemble-volume-change-threshold)
  ((density-trend :init) (map (fn [x] 0) (range INITIAL-NUM-DENSITY-TREND)) @ensemble-density-change-threshold)
  ((pitch-trend :init) (map (fn [x] midi-mid) (range INITIAL-NUM-PITCH-TREND)) @ensemble-pitch-change-threshold)

  (reset! player-densities (apply vector (repeat @number-of-players 0)))
  (reset! player-note-durs (apply vector (repeat @number-of-players 0)))
  (reset! player-notes (apply vector (repeat @number-of-players midi-mid)))

  ;; update ensemble-status with each new note
  (register-listener
   MSG-PLAYER-NEW-NOTE
   transport.ensemble-status/update-ensemble-status
   {}
   )
  (sched-event trend-upd-millis transport.ensemble-status/upd-ensemble-trends nil)
  )

(defn reset-ensemble-status
  []
  (init-ensemble-status)
  )
