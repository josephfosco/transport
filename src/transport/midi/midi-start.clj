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

;; To make this work
;;
;; .jackdrc in home directory should contain -Xnone to not load midi drivers
;; mine looks like:
;; /usr/bin/jackd -dalsa -dhw:0 -r44100 -p1024 -n2 -Xnone -D
;;
;; in the terminal install virtual midi drivers using the command:
;; sudo modprobe snd-virmidi snd_index=1
;; snd_index=1 if there is only 1 other audio device
;;
;; start Virtual MIDI Piano Keyboard (VMPK)
;;
;; start transport (lein repl - in transport directory)
;;
;; in VMPK goto Edit - Midi Connections
;; in the popup for Output MIDI Connection select Virtual Raw MIDI 1-0-0



(ns transport.midi.midi-start
  (:use [overtone.live]
   )
  (:require [overtone.midi :as midi])
  )

(defn midi-print [event]
  (println (:note event) (/ (:velocity event) 127.0))
  )

(def kb (midi/midi-find-device (midi/midi-sources) "hw:1,0,0"))

(def keyboard (midi/midi-in kb))

(midi/midi-handle-events keyboard #'midi-print)





(first (midi/midi-sources))

(first (midi-find-connected-devices "hw:1,0,0"))

(def kb (midi-find-connected-device "hw:1,0,0"))

(println kb)

(midi-device-num kb)

(midi/midi-handle-events kb #(midi-print %1))

(on-event [:midi :note-on] (fn [{note :note velocity :velocity}]
                             (println "Note: " note ", Velocity: " velocity))
          ::note-printer)

(remove-event-handler ::note-printer)




(def kb (midi/midi-find-device (midi/midi-sources) "VirMIDI, VirMidi, Virtual Raw MIDI"))

(midi/midi-find-device (midi/midi-sources) "VirMIDI, VirMidi, Virtual Raw MIDI")

(midi/midi-ports)

(definst ding
  [note 60 velocity 100]
  (let [freq (midicps note)
        snd  (sin-osc freq)
        env  (env-gen (perc 0.1 0.8) :action FREE)]
    (* velocity env snd)))

(defn midi-player [event]
  (ding (:note event) (/ (:velocity event) 127.0)))

(midi/midi-handle-events keyboard #'midi-player)
