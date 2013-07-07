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
   [transport.random :only [random-dur random-int]]))

(def min-mm 40)
(def max-mm 200)

(def base-dur 1)
(def quarter-note 8)

(def NOTE-DURS
  [(int (* quarter-note (/ 1 8))),        ; 32nd note - 1
   (int (* quarter-note (/ 1 4))),        ; 16th note - 2
   (int (+ (* quarter-note (/ 1 4)) (* quarter-note (/ 1 8)))),    ; dotted 32nd note - 3
   (int (* quarter-note (/ 1 2))),        ; 8th note - 4
   (int (+ (* quarter-note (/ 1 2)) (* quarter-note (/ 1 4)))),    ; dotted 8th note - 6
   quarter-note,                          ; quarter note - 8
   (+ quarter-note (/ quarter-note 2)),   ; dotted quarter - 12
   (* quarter-note 2),                    ; half - 16
   (* quarter-note 3),                    ; dotted half - 24
   (* quarter-note 4)                     ; whole - 32
   (* quarter-note 8)                     ; double whole - 64
   ])

(defn note-dur-to-millis [player note-dur]
  (int (* (* (/ 60 (:mm player)) (/ note-dur quarter-note ))  1000)))

(defn pick-mm [player]
  (random-int min-mm max-mm))

(defn next-note-dur
  "Return the duration of the next note in milliseconds"
  [ player ]
   ;  (random-dur 10 4000)
  (note-dur-to-millis player (NOTE-DURS (random-dur 0 (- (count NOTE-DURS) 1))))
  )
