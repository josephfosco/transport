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

(ns transport.ensemble
  (:use
   [overtone.live]
   [transport.behavior :only [get-behavior-action-for-player select-and-set-behavior-player-id]]
   [transport.debug :only [debug-run1]]
   [transport.ensemble-status :only [update-ensemble-status]]
   [transport.instrument :only [get-instrument play-instrument]]
   [transport.melody :only [get-volume next-melody]]
   [transport.messages]
   [transport.message_processor :only [send-message]]
   [transport.players]
   [transport.rhythm :only [get-beats]]
   [transport.schedule :only [sched-event]]
   [transport.segment :only [copy-following-info first-segment new-segment]]
   [transport.settings :only [NUM-PLAYERS SAVED-MELODY-LEN FOLLOW CONTRAST]]
   ))

(defn play-melody
  "Gets the note to play now and plays it (if it is not a rest)
   Checks if the current segment is done, and if so
   sets up a new one.
   Then schedules the next note to play

   player - map for the current player
   event-time - time this note event was scheduled for"
  [player-id event-time]
  (let [
        player (get-player player-id)
        player-action (get-behavior-action-for-player player)
        melody-event (next-melody player )
        melody-dur-millis (get-dur-millis (:dur-info melody-event))
        prev-note-beat (:cur-note-beat player)
        cur-note-beat (if (not (nil? (:dur-info melody-event)))
                        (+ (:cur-note-beat player) (get-beats (:dur-info melody-event)))
                        0)
        ;; if seg-start = 0 this is the begining of the segment, so
        ;; set seg-start to the time of this event - also send seg-start msg
        seg-start-time (if (= (:seg-start player) 0)
                         (do
                           (send-message MSG-PLAYER-NEW-SEGMENT :change-player-id (get-player-id player))
                           event-time)
                         (:seg-start player))
        ]

    (if (not (nil? (:note melody-event)))
      (play-instrument player (:note melody-event) melody-dur-millis (get-volume melody-event)))
    (if (nil? melody-dur-millis)
      (println "MELODY EVENT :DUR IS NILL !!!!"))
    ;; If current segment is over, sched next event with a new segment
    ;; else sched event with current segment information
    (let [upd-player
          (if (< (+ seg-start-time (:seg-len player)) event-time)
            (new-segment (assoc player
                           :cur-note-beat cur-note-beat
                           :prev-note-beat prev-note-beat))
            (assoc player
              :cur-note-beat cur-note-beat
              :melody (let [cur-melody (get-melody player)
                            next-key   (if (> (count cur-melody) 0)
                                         (+ (reduce max (keys cur-melody)) 1)
                                         1)]
                        (if (= (count cur-melody) SAVED-MELODY-LEN)
                          (do
                            (assoc (dissoc cur-melody (- next-key SAVED-MELODY-LEN))
                              next-key  melody-event)
                            )
                          (assoc cur-melody next-key melody-event)
                          ))
              :prev-note-beat prev-note-beat
              :seg-start seg-start-time
              ))
          ]
      (sched-event melody-dur-millis (get-function upd-player) (get-player-id upd-player))
      (update-player upd-player)
      (update-ensemble-status upd-player)
      )))

(defn create-player
  [player-no]
  (first-segment{:cur-note-beat 0
               :function transport.ensemble/play-melody
               :melody {}
               :player-id player-no
               :prev-note-beat 0}))

(defn init-ensemble
  []
  (let [all-players (map create-player (range 1 (+ @NUM-PLAYERS 1)))]
    (reset-players)
    (send PLAYERS conj (zipmap (map get all-players (repeat :player-id)) all-players))
    (await PLAYERS)
    )

  ;; set the :behavior :player-id for all players that are FOLLOWING or COMPLEMENT
  ;; then, for all players that are FOLLOWING
  ;;  reset :instrument-info to that of the player they are FOLOWING
  (let [final-players
        (zipmap
         (keys @PLAYERS)
         (map assoc (vals @PLAYERS) (repeat :behavior) (map select-and-set-behavior-player-id (vals @PLAYERS))))
        ]
    (reset-players)
    (send PLAYERS conj final-players)
    (await PLAYERS)
    )

  (init-players)

  (dorun (map print-player (get-players)))

  ;; Schedule first event for all players
   (dorun (map sched-event (repeat 0) (map get-function (get-players)) (map get-player-id (get-players))))
  )
