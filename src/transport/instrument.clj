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
   [transport.random :only [random-int]]))

(defn note->hz [music-note]
  (midi->hz music-note))

(definst triangle-wave [freq 440 attack 0.01 sustain 0.1 release 0.4 vol 0.4]
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (lf-tri freq)
     vol))

(defn play-instrument [instrument music-note]
  (instrument (midi->hz music-note)))

(defn choose-range [player]
  (let [lo (random-int (first MIDI-RANGE) (last MIDI-RANGE))
        hi (if (= lo (last MIDI-RANGE)) (last MIDI-RANGE) (random-int lo (last MIDI-RANGE)))]
    (list lo hi)
    ))

(defn pick-instrument [player]
  (let [inst-range (choose-range player)]
    {:instrument transport.instrument/triangle-wave,
     :range-hi (last inst-range)
     :range-lo (first inst-range)}))

(defn get-instrument [player]
  (:instrument (:instrument-info player)))

(defn get-hi-range [player]
  (:range-hi (:instrument-info player)))

(defn get-lo-range [player]
  (:range-lo (:instrument-info player)))

(defn get-instrument-range [player]
  (list (get-lo-range player) (get-hi-range player)))
