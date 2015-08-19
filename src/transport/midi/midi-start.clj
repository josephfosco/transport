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

(def kb (midi/midi-find-device (midi/midi-sources) "VirMIDI [hw:1,0,0]"))

(def keyboard (midi/midi-in kb))

(midi/midi-handle-events keyboard #'midi-print)






(first (midi-find-connected-devices "hw:1,0,0"))


(def kb (midi-find-connected-device "hw:1,0,0"))

(println kb)

kb

(midi-device-num kb)

(midi/midi-handle-events kb #(midi-print %1))

(on-event [:midi :note-on] (fn [{note :note velocity :velocity}]
                             (println "Note: " note ", Velocity: " velocity))
          ::note-printer)

(remove-event-handler ::note-printer)


(def kb (midi/midi-find-device (midi/midi-sources) "VirMIDI [hw:1,0,0]"))

(def kb (midi/midi-find-device (midi/midi-sources) "VirMIDI, VirMidi, Virtual Raw MIDI"))

(midi/midi-find-device (midi/midi-sources) "VirMIDI, VirMidi, Virtual Raw MIDI")

kb

(midi/midi-in kb)

(let [receiver (proxy [Receiver] [] (close [] nil) (send [msg timestamp] (#'midi-print (midi-msg msg) timestamp)))])

(midi/midi-ports)

(definst ding
  [note 60 velocity 100]
  (let [freq (midicps note)
        snd  (sin-osc freq)
        env  (env-gen (perc 0.1 0.8) :action FREE)]
    (* velocity env snd)))

(defn midi-player [event]
  (ding (:note event) (/ (:velocity event) 127.0)))

(midi-handle-events kb #'midi-player)
