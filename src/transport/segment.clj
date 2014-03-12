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

(ns transport.segment
  (:use
   [transport.behavior :only [get-behavior-action select-behavior]]
   [transport.instrument :only [get-instrument-info select-instrument]]
   [transport.melody :only [select-melody-characteristics]]
   [transport.pitch :only [select-key select-scale]]
   [transport.players]
   [transport.random :only [random-int]]
   [transport.rhythm :only [select-metronome select-mm]]))

(def min-segment-len 10000)  ;minimum segment length in milliseconds (10 seconds)
(def max-segment-len 30000)  ;maximum segment length in milliseconds (30 seconds)

(defn select-segment-length
  []
  (random-int min-segment-len max-segment-len))

(defn copy-following-info
  [player]
  (let [follow-player (get-player (get-behavior-player-id player))]
    (assoc player
      :instrument-info (get-instrument-info follow-player)
      :key (get-key follow-player)
      :melody-char (get-melody-char follow-player)
      :metronome (get-metronome follow-player)
      :mm (get-mm follow-player)
      :scale (get-scale follow-player)
      ))
  )

(defn first-segment
  [player]
  (let [new-behavior (transport.behavior/select-behavior player)
        melody-len (count (get-melody player))
        behavior-action (get-behavior-action new-behavior)
        ]
    (assoc player
      :behavior new-behavior
      ;; set instrument-info after :behavior so :instrument-info
      ;; is copied from FOLLOWing player if required
      :instrument-info (select-instrument player new-behavior)
      :key (select-key player)
      :melody-char (select-melody-characteristics player)
      :metronome (select-metronome player)
      :mm (select-mm player)
      :seg-len (select-segment-length)
      :seg-start 0
      :scale (select-scale player))))

(defn new-segment
  [player]
  (let [new-behavior (transport.behavior/select-behavior player)
        melody-len (count (get-melody player))
        behavior-action (get-behavior-action new-behavior)
        ]
    (assoc player
      :behavior new-behavior
      ;; set instrument-info after :behavior so :instrument-info
      ;; is copied from FOLLOWing player if required
      :instrument-info (select-instrument player new-behavior)
      :key (select-key player)
      :melody-char (select-melody-characteristics player)
      :metronome (select-metronome player)
      :mm (select-mm player)
      :seg-len (select-segment-length)
      :seg-start 0
      :scale (select-scale player))))
