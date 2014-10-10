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

(ns transport.instrument
  (:require
   [overtone.live :refer :all]
   [transport.behaviors :refer [get-behavior-action-for-player get-behavior-player-id-for-player]]
   [transport.instruments.elec-instruments :refer :all]
   [transport.instruments.misc-instruments :refer :all]
   [transport.instruments.osc-instruments :refer :all]
   [transport.instruments.pitched-perc-instruments :refer :all]
   [transport.instruments.trad-instruments :refer :all]
   [transport.players :refer [get-instrument-info get-player get-player-id print-player]]
   [transport.settings :refer :all]
   [transport.random :refer [random-int]]
   [transport.util :refer :all]
   ))

(def LO-RANGE 47)
(def MID-RANGE 79)
(def HI-RANGE (last MIDI-RANGE))

(def all-instruments [
;                      {:instrument triangle-wave :envelope-type "AD"}
                      {:instrument bass-m1
                       :envelope-type "NE"
                       :range-lo 0 :range-hi 60}
                      {:instrument bassoon
                       :envelope-type "ASR"
                       :range-lo 0 :range-hi 84}
                      {:instrument drum-m1
                       :envelope-type "AD"
                       :range-lo (first MIDI-RANGE) :range-hi 90}
                      {:instrument organ-m1
                       :envelope-type "NE"
                       :range-lo (first MIDI-RANGE)
                       :range-hi (last MIDI-RANGE)}
                      {:instrument plink-m1
                       :envelope-type "AD"
                       :range-lo (first MIDI-RANGE)
                       :range-hi (last MIDI-RANGE)}
                      {:instrument reedy-organ
                       :envelope-type "ASR"
                       :range-lo (first MIDI-RANGE)
                       :range-hi (last MIDI-RANGE)}
                      {:instrument saw-wave-sus
                       :envelope-type "ASR"
                       :range-lo (first MIDI-RANGE) :range-hi (last MIDI-RANGE)}
                      {:instrument sine-wave-sus
                       :envelope-type "ASR"
                       :range-lo (first MIDI-RANGE) :range-hi (last MIDI-RANGE)}
                      {:instrument steel-drum
                       :envelope-type "AD"
                       :range-lo (first MIDI-RANGE) :range-hi (last MIDI-RANGE)}
                      {:instrument tri-wave-sus
                       :envelope-type "ASR"
                       :range-lo (first MIDI-RANGE) :range-hi (last MIDI-RANGE)}
                      ])

(defn note->hz
  [music-note]
  (midi->hz music-note))

(defn get-instrument
  [player]
  (:instrument (get-instrument-info player)))

(defn get-instrument-range-hi
  [instrument-info]
  (:range-hi instrument-info))

(defn get-instrument-range-lo
  [instrument-info]
  (:range-lo instrument-info))

(defn get-hi-range
  [player]
  (:range-hi (get-instrument-info player)))

(defn get-lo-range
  [player]
  (:range-lo (get-instrument-info player)))

(defn get-instrument-range-for-player
  [player]
  (list (get-lo-range player) (get-hi-range player)))

(defn get-envelope-type
  [player]
  (:envelope-type (get-instrument-info player)))

(defn get-gate-dur
  "player - player map
   note-duration - note duration in milliseconds"
  [player note-duration]
  (if (> note-duration 110)
    (/ (- note-duration 110) 1000.0) ; currently assumes an attack of .01 secs and decay of .1 secs
    0.001))

(defn select-random-instrument
  "Selects random instrument-info for player.
   Can be used the first time instrument-info is set."
  []
  (let [inst-info (rand-nth all-instruments)
        ]
    {:instrument (:instrument inst-info)
     :envelope-type (:envelope-type inst-info)
     :range-hi (:range-hi inst-info)
     :range-lo (:range-lo inst-info)}
    ))

(defn select-instrument
  "Selects a random instrument-info for player.
   Generally this should not be used if player is FOLLOWing
   another player -in that case the instrument-info should be copied
   from the player that is being FOLLOWed

   player - the player to get instrument for"
  [player]
  (let [behavior-action (get-behavior-action-for-player player)
        cntrst-plyr (if (= behavior-action CONTRAST-PLAYER)
                      (get-player (get-behavior-player-id-for-player player))
                      nil)
        ;; select instrument info from all-insruments map
        ;; if not CONTRASTing, select a random instrument
        ;; if CONTRASTing select an instrument other than the one CONTRAST player is using
        inst-info (if (not= behavior-action CONTRAST-PLAYER)
                    (rand-nth all-instruments)
                    (let [instrument-index (rand-int (count all-instruments))]
                      (if (=
                           (:name (:instrument (get-instrument-info cntrst-plyr)))
                           (:name (:instrument (nth all-instruments instrument-index))))
                        (nth all-instruments (mod
                                              (+ instrument-index (rand-int (dec (count all-instruments))))
                                              (dec(count all-instruments))))
                        (nth all-instruments instrument-index)
                        )
                      )
                    )
        ]
    {:instrument (:instrument inst-info)
     :envelope-type (:envelope-type inst-info)
     :range-hi (:range-hi inst-info)
     :range-lo (:range-lo inst-info)}
    )
  )

(defn- check-note-out-of-range
  [player note-num]
  (if (and
       (not= (get-behavior-action-for-player FOLLOW-PLAYER))
       (or (< note-num (:range-lo (get-instrument-info player))) (> note-num (:range-hi (get-instrument-info player)) )))
    (do
      (print-banner "instrument.clj - play-instrument-asr - NOTE OUT OF INSTRUMENT RANGE")
      (println "player instrument:" (:name (:instrument (:instrument-info player))) "note-num:")
      (print-player player)
      (println "FOLLOWING PLAYER")
      (print-player (get-player (get-behavior-player-id-for-player player)))
      (print-banner "end instrument.clj - play-instrument-asr - NOTE OUT OF INSTRUENT RANGE end")
      ))
  )

(defn get-instrument-for-note
  [player]
  (get-instrument player)
  )

(defn play-instrument-no-env
  "player - player map
   note-num - midi note number
   note-duration - note duration in milliseconds
   volume - the volume to play this note"
  [player note-num note-duration volume]
  (let [instrument (get-instrument-for-note player)]

    (instrument (midi->hz note-num) note-duration volume)
    (check-note-out-of-range player note-num)
    ))

(defn play-instrument-ar
  "player - player map
   note-num - midi note number
   note-duration - note duration in milliseconds
   volume - the volume to play this note"
  [player note-num note-duration volume]
  (let [instrument (get-instrument-for-note player)]

    (instrument (midi->hz note-num) volume)
    (check-note-out-of-range player note-num)
    ))

(defn play-instrument-asr
  "player - player map
   note-num - midi note number
   note-duration - note duration in milliseconds
   volume - the volume to play this note"
  [player note-num note-duration volume]
  (let [gate-duration (get-gate-dur player note-duration)
        instrument (get-instrument player)
        ]
    (instrument (midi->hz note-num) gate-duration volume)
    (check-note-out-of-range player note-num)
    ))

(defn play-instrument
  "player - player map
   note-num - midi note number
   note-duration - note duration in milliseconds
   volume - the volume to play this note"
  [player note-num note-duration volume]
  (let [ env-type (get-envelope-type player)]
    (cond
     (.equals env-type "AD") (play-instrument-ar player note-num note-duration volume)
     (.equals env-type "ASR") (play-instrument-asr player note-num note-duration volume)
     (.equals env-type "NE") (play-instrument-no-env player note-num note-duration volume)
     :else ((get-instrument player) (midi->hz note-num)))))
