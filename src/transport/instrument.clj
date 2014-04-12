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
   [transport.instruments.osc-instruments :refer :all]
   [transport.players :refer [get-instrument-info get-player]]
   [transport.settings :refer :all]
   [transport.random :refer [random-int]]
   ))

(def LO-RANGE 47)
(def MID-RANGE 79)
(def HI-RANGE (last MIDI-RANGE))

(def all-instruments [
;                      {:instrument triangle-wave :envelope-type "AD"}
                      {:instrument tri-wave-sus :envelope-type "ASR"}
                      {:instrument saw-wave-sus :envelope-type "ASR"}
                      {:instrument sine-wave-sus :envelope-type "ASR"}
                      ])

(defn note->hz
  [music-note]
  (midi->hz music-note))

(defn get-instrument
  [player]
  (:instrument (get-instrument-info player)))

(defn get-hi-range
  [player]
  (:range-hi (get-instrument-info player)))

(defn get-lo-range
  [player]
  (:range-lo (get-instrument-info player)))

(defn get-instrument-range
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

(defn select-random-range
  []
  (let [lo (random-int (first MIDI-RANGE) (last MIDI-RANGE))
        hi (if (= lo (last MIDI-RANGE)) (last MIDI-RANGE) (random-int lo (last MIDI-RANGE)))]
    (list lo hi)
    ))

(defn- select-contrasting-range
  "If CONTRAST player is in low range, will return a high range
   If CONTRAST player is in high range, will return a lower range
   If CONTRAST player has wide range, will return a narrow range"
  [player cntrst-plyr]
  (let [cntrst-plyr-lo (get-lo-range cntrst-plyr)
        cntrst-plyr-hi (get-hi-range cntrst-plyr)
        lowest-note (cond
                     (<= cntrst-plyr-hi LO-RANGE) (+ LO-RANGE 1)
                     (<= cntrst-plyr-hi MID-RANGE) (+ cntrst-plyr-lo 1)
                     :else (first MIDI-RANGE)
                     )
        highest-note (cond
                      ;; if cntrst-plyr has wide range then set to narrow range
                      (> (- cntrst-plyr-hi cntrst-plyr-lo) (* OCTAVE 4)) (+ lowest-note (- OCTAVE 1))
                      (<= cntrst-plyr-hi MID-RANGE) (last MIDI-RANGE)
                     :else (last MIDI-RANGE)
                      )
        lo (random-int lowest-note highest-note)
        hi (if (= lo highest-note) highest-note (random-int lowest-note highest-note))
        ]
    (list lo hi)
    )
  )

(defn- select-range
  [player]
  (let [lo (random-int (first MIDI-RANGE) (last MIDI-RANGE))
        hi (if (= lo (last MIDI-RANGE)) (last MIDI-RANGE) (random-int lo (last MIDI-RANGE)))]
    (list lo hi)
    )
  )

(defn select-random-instrument
  "Selects random instrument-info for player.
   Can be used the first time instrument-info is set."
  []
  (let [inst-range (select-random-range)
        ;; select instrument info from all-insruments map
        inst-info (rand-nth all-instruments)
        ]
    {:instrument (:instrument inst-info)
     :envelope-type (:envelope-type inst-info)
     :range-hi (last inst-range)
     :range-lo (first inst-range)}
    ))

(defn select-instrument
  "Selects a random instrument-info for player.
   Generally this should not be used if player is FOLLOWing
   another player -in that case the instrument-info should be copied
   from the player that is being FOLLOWed

   player - the player to get instrument for"
  [player]
  (let [behavior-action (get-behavior-action-for-player player)
        cntrst-plyr (if (= behavior-action CONTRAST)
                      (get-player (get-behavior-player-id-for-player player))
                      nil)
        inst-range (if (= behavior-action CONTRAST)
                     (select-contrasting-range player cntrst-plyr)
                     (select-range player))
        ;; select instrument info from all-insruments map
        ;; if not CONTRASTing, select a random instrument
        ;; if CONTRASTing select an instrument other than the one CONTRAST player is using
        inst-info (if (not= behavior-action CONTRAST)
                    (rand-nth all-instruments)
                    (let [instrument-index (rand-int (count all-instruments))]
                      (if (=
                           (:name (:instrument (get-instrument-info cntrst-plyr)))
                           (:name (:instrument (nth all-instruments instrument-index))))
                        (nth all-instruments (mod
                                              (+ instrument-index (rand-int (dec (count all-instruments))))
                                              (dec (count all-instruments))))
                        (nth all-instruments instrument-index)
                        )
                      )
                    )
        ]
    {:instrument (:instrument inst-info)
     :envelope-type (:envelope-type inst-info)
     :range-hi (last inst-range)
     :range-lo (first inst-range)}
    )
  )

(defn play-instrument-asr
  "player - player map
   note-num - midi note number
   note-duration - note duration in milliseconds
   volume - the volume to play this note"
  [player note-num note-duration volume]
  (let [gate-duration (get-gate-dur player note-duration)]
    ((get-instrument player) (midi->hz note-num) gate-duration volume)
    ))

(defn play-instrument
  "player - player map
   note-num - midi note number
   note-duration - note duration in milliseconds
   volume - the volume to play this note"
  [player note-num note-duration volume]
  (let [ env-type (get-envelope-type player)]
    (cond
     (.equals env-type "ASR") (play-instrument-asr player note-num note-duration volume)
     :else ((get-instrument player) (midi->hz note-num)))))
