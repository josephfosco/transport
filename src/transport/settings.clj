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

(ns transport.settings
  "This namespace is a 'terminal namespace'.
   It should not :require :use :refer or :import any
   other namespaces")

(def number-of-players (atom 10))
(def SAVED-MELODY-LEN 64)
(def OCTAVE 12)

(def IGNORE 0)
(def CONTRAST 1)
(def SIMILAR 2)
(def FOLLOW 3)

(def STEADY 10)
(def INCREASING 11)
(def DECREASING 12)
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

(defn set-number-of-players
  [new-num-players]
  (reset! number-of-players new-num-players))
