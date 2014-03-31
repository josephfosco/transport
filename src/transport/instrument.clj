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

(ns transport.instrument
  (:use
   [overtone.live]
   [transport.behaviors :only [get-behavior-action-for-player get-behavior-player-id-for-player]]
   [transport.instruments.osc-instruments]
   [transport.players :only [get-instrument-info get-player]]
   [transport.settings :only [FOLLOW]]
   [transport.random :only [random-int]]
   ))

(defn note->hz
  [music-note]
  (midi->hz music-note))

(def all-instruments [
;                      {:instrument triangle-wave :envelope-type "AD"}
                      {:instrument tri-wave-sus :envelope-type "ASR"}
                      {:instrument saw-wave-sus :envelope-type "ASR"}
                      {:instrument sine-wave-sus :envelope-type "ASR"}
                      ])

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

(defn select-range
  [player]
  (let [lo (random-int (first MIDI-RANGE) (last MIDI-RANGE))
        hi (if (= lo (last MIDI-RANGE)) (last MIDI-RANGE) (random-int lo (last MIDI-RANGE)))]
    (list lo hi)
    ))

(defn select-instrument
  [player behavior]
  ;; if :behavior is FOLLOW copy :inst-info from player we are following
  ;; else generate new :inst-info map
  (if (and  (= (get-behavior-action-for-player (hash-map :behavior behavior)) FOLLOW)
            (not= (get-behavior-player-id-for-player (hash-map :behavior behavior)) nil))
    (do
      (get-instrument-info (get-player (get-behavior-player-id-for-player (hash-map :behavior behavior)))))
    (let [inst-range (select-range player)
          ;; select instrument info from all-insruments map
          inst-info (nth all-instruments (random-int 0 (- (count all-instruments) 1)))
          ]
      {:instrument (:instrument inst-info)
       :envelope-type (:envelope-type inst-info)
       :range-hi (last inst-range)
       :range-lo (first inst-range)}
      )))

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
