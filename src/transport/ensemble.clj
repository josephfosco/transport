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

(ns transport.ensemble
  (:use
   [overtone.live]
   [transport.behavior :only [get-behavior get-behavior-action get-behavior-player-id select-and-set-behavior-player-id]]
   [transport.debug :only [debug-run1]]
   [transport.instrument :only [get-instrument play-instrument]]
   [transport.melody :only [next-melody]]
   [transport.players :only [PLAYERS reset-players update-player]]
   [transport.random :only [FOLLOW CONTRAST]]
   [transport.rhythm :only [get-beats get-dur-millis]]
   [transport.schedule :only [sched-event]]
   [transport.segment :only [new-segment]]
   [transport.settings :only [NUM-PLAYERS]]
   [transport.util])
   )

(defn play-melody
  "player - map for the current player
   event-time - time this note event was scheduled for

   Gets the note to play now and plays it (if it is not a rest)
   Checks if the current segment is done, and if so
   sets up a new one.
   Then schedules the next note to play"
  [player event-time]
  (let [
        player-action (get-behavior-action player)
        melody-event (next-melody player )
        melody-dur-millis (get-dur-millis (:dur-info melody-event))
        prev-note-beat (:cur-note-beat player)
        cur-note-beat (if (not (nil? (:dur-info melody-event)))
                        (+ (:cur-note-beat player) (get-beats (:dur-info melody-event)))
                        0)
        ;; if seg-start = 0 this is the begining of the segment, so
        ;; set seg-start to the time of this event
        seg-start-time (if (= (:seg-start player) 0) event-time (:seg-start player))
        ]

    (if (not (nil? (:note melody-event)))
      (play-instrument player (:note melody-event) melody-dur-millis ))
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
              :melody (conj (:melody player) melody-event)
              :prev-note-beat prev-note-beat
              :seg-start seg-start-time
              ))
          ]
      (sched-event melody-dur-millis upd-player)
      (send-off PLAYERS update-player upd-player)
      )
    ))

(defn create-player
  [player-no]
  (new-segment{:cur-note-beat 0
               :function transport.ensemble/play-melody
               :player-id player-no
               :prev-note-beat 0}))

(defn init-players
  []
  (let [all-players (map create-player (range 1 ( + NUM-PLAYERS 1)))]
    (reset-players)

    (send-off PLAYERS conj (zipmap (map get all-players (repeat :player-id)) all-players))
    (await PLAYERS)
    )

  ;; set the :behavior :player-id for all players that are FOLLOWING
  (let [final-players
        (zipmap
         (keys @PLAYERS)
         (map assoc (vals @PLAYERS) (repeat :behavior) (map select-and-set-behavior-player-id (vals @PLAYERS))))
        ]
    (reset-players)
    (send-off PLAYERS conj final-players)
    (await PLAYERS)
    )

  (dotimes [player-index NUM-PLAYERS]
    (sched-event 0 (get @PLAYERS (+ player-index 1)))
    )
  )
