;    Copyright (C) 2015  Joseph Fosco. All Rights Reserved
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

(ns transport.midi.midi-start
  (:use [overtone.live]
   )
  (:require [overtone.midi :as midi])
  )

(defn midi-print [event]
  ;; (println (:note event) (/ (:velocity event) 127.0))
  (println "MIDI IN")
  )

(first (midi-find-connected-devices "hw:1,0,0"))


(def kb (first (midi-find-connected-devices "hw:1,0,0")))

(println kb)

(midi/midi-in kb)

kb

(midi-device-num kb)

(midi/midi-handle-events kb #'midi-print)

(on-event [:midi :note-on] (fn [{note :note velocity :velocity}]
                             (println "Note: " note ", Velocity: " velocity))
          ::note-printer)

(remove-event-handler ::note-printer)
