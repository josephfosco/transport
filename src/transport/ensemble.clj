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

(defn play-melody [player]
  (if (< (count (:melody player)) (:num-notes player))
    (do
      (let [melody-event (next-melody player)]
        (if (not (nil? (:note melody-event)))
          (play-instrument (get-instrument player) (:note melody-event)))
        (sched-event (:dur melody-event)
                     (assoc player
                   ;    :dur (:dur melody-event)
                       :melody (conj (:melody player) melody-event)
                     ))))))

(defn create-player_orig [player-no]
  (let [new-player {}]
    (new-segment
     (assoc new-player
       :function transport.ensemble/play-melody,
       :player-id player-no)))
  )

(defn create-player [player-no]
  (new-segment{:function transport.ensemble/play-melody,
               :player-id player-no})
  )

(defn init-players_orig []
  (dotimes  [player-index  NUM-PLAYERS]
    (play-melody (create-player (+ player-index 1))) ))

(defn init-players []
  (dotimes  [player-index  NUM-PLAYERS]
    (sched-event ;(rand-int 2000)
     (:dur 0)
     (create-player (+ player-index 1))) ))
