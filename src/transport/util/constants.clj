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

(ns transport.util.constants
  "This namespace is a 'terminal namespace'.
   It should not :require :use :refer or :import any
   other transport namespaces"
  (:require
   [overtone.live :refer [MIDI-RANGE]]
   )
  )

(def SAVED-MELODY-LEN 64)
(def OCTAVE 12)
(def MIDI-HI (last MIDI-RANGE))

(def IGNORE-ALL 0)
(def CONTRAST-PLAYER 1)
(def SIMILAR-PLAYER 2)
(def FOLLOW-PLAYER 3)
(def SIMILAR-ENSEMBLE 4)
(def CONTRAST-ENSEMBLE 5)

(def DECREASING 10)
(def STEADY 11)
(def INCREASING 12)
(def RANDOM 13)
(def SHORT 14)
(def LONG 15)
(def RAPID 16)
(def SLOW 17)
(def KEEP-BEAT 18)

(def SLOW-TEMPO [0 60])
(def MED-TEMPO [61 100])
(def FAST-TEMPO [101 160])
(def VERY-FAST-TEMPO [161 999])
