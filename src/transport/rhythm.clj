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

(ns transport.rhythm
  (:use
   [transport.random :only [random-dur random-int]]
   [overtone.live :only [metronome]]
   ))

(def min-mm 40)
(def max-mm 200)

(def base-dur 1)
(def quarter-note 8)

(def NOTE-DURS
  [(int (* quarter-note (/ 1 8)))         ; 32nd note - 1
   (int (* quarter-note (/ 1 4)))         ; 16th note - 2
   (int (+ (* quarter-note (/ 1 4)) (* quarter-note (/ 1 8))))    ; dotted 32nd note - 3
   (int (* quarter-note (/ 1 2)))         ; 8th note - 4
   (int (+ (* quarter-note (/ 1 2)) (* quarter-note (/ 1 4))))    ; dotted 8th note - 6
   quarter-note                           ; quarter note - 8
   (+ quarter-note (/ quarter-note 2))    ; dotted quarter - 12
   (* quarter-note 2)                     ; half - 16
   (* quarter-note 3)                     ; dotted half - 24
   (* quarter-note 4)                     ; whole - 32
   (* quarter-note 8)                     ; double whole - 64
   ])

(def NOTE-PROBS
  ;;2 8 5 15 10 15 10 15 10 5 5
  [2 10 15 30 40 55 65 80 90 95 100]
)

(defn note-dur-to-millis
  "Converts note-dur (in beats) to millis at the mm for player

   player - player to get mm for
   note-dur - note duration in beats"
  [player note-dur]
  (int (* (* (/ 60.0 (:mm player)) note-dur)  1000))
  )

(defn millis-to-note-dur
  [player millis]
  (/ millis (* (/ 60.0 (:mm player)) 1000)))

(defn get-dur-millis
  "Returns the millis duraition for the dur-info

   dur-info - duration info to get millis from"
  [dur-info]
  (:dur-millis dur-info)
  )

(defn get-beats
  "Returns the duration in beats of this dur-info

   dur-info - duration info to get dur-beats from"
  [dur-info]
  (:dur-note-dur dur-info)
  )

(defn select-mm
  [player]
  (random-int min-mm max-mm)
  60
  )

(defn select-metronome
  [player]
  (metronome (:mm player)))

(defn get-dur-info-for-beats
  "Returns :dur-info map with
     :dur-note-dur = beats
     :dur-millis = beats converted to milliseconds

   player - player map to use to convert beats to milliseconds
   beats - the number of beats to use in dur-info and
             convert to milliseconds"
  [player beats]
  {:dur-millis (note-dur-to-millis player (* quarter-note beats))
   :dur-note-dur beats}
  )

(defn next-note-dur
  "Returns :dur-info map for the next note

   player - player map to use when determining next note :dur-info"
  [ player ]
  (let [note-prob (random-int 0 99)
        note-dur (cond
                  (< note-prob (NOTE-PROBS 0)) 0
                  (< note-prob (NOTE-PROBS 1)) 1
                  (< note-prob (NOTE-PROBS 2)) 2
                  (< note-prob (NOTE-PROBS 3)) 3
                  (< note-prob (NOTE-PROBS 4)) 4
                  (< note-prob (NOTE-PROBS 5)) 5
                  (< note-prob (NOTE-PROBS 6)) 6
                  (< note-prob (NOTE-PROBS 7)) 7
                  (< note-prob (NOTE-PROBS 8)) 8
                  (< note-prob (NOTE-PROBS 9)) 9
                  :else 10)
        ]
    {:dur-millis (note-dur-to-millis player (/ (NOTE-DURS note-dur) quarter-note))
     :dur-note-dur (/ (NOTE-DURS note-dur) quarter-note)}
    ))

;; (+ (tempm1 0) (beat-ms 1.5 (metro-bpm tempm1)))
;; takes some beat (0 in this case) and adds some number of beats to it
;; returns the number of milliseconds past the first beat (0 in this case)
