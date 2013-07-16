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
   [transport.debug :only [debug-run1]]
   [transport.instrument :only [get-instrument play-instrument]]
   [transport.melody :only [next-melody]]
   [transport.schedule :only [sched-event]]
   [transport.segment :only [new-segment]]))

(def NUM-PLAYERS 10)

(defn play-melody [player event-time]
  "player - map for the current player
   event-time - time this note event was scheduled for

   Gets the note to play now and plays it (if it is not a rest)
   Checks if the current segment is done, and if so
   sets up a new one.
   Then schedules the next note to play"
  (if (< (count (:melody player)) (:num-notes player))
    (do
      (let [melody-event (next-melody player)]
        (if (not (nil? (:note melody-event)))
          (play-instrument (get-instrument player) (:note melody-event)))
        (if (nil? (:dur melody-event))
          (println "MELODY EVENT :DUR IS NILL !!!!"))
        (sched-event (:dur melody-event)
                     (assoc player
                       :melody (conj (:melody player) melody-event)
                       :seg-start (if (= (:seg-start player) 0)
                                    event-time
                                    (:seg-start player))
                     ))))))

(defn create-player [player-no]
  (new-segment{:function transport.ensemble/play-melody,
               :player-id player-no}))

(defn init-players []
  (dotimes  [player-index  NUM-PLAYERS]
    (sched-event
     (:dur 0)
     (create-player (+ player-index 1)))))
