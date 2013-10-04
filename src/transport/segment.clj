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

(ns transport.segment
  (:use
   [transport.instrument :only [select-instrument]]
   [transport.pitch :only [select-key select-scale]]
   [transport.random :only [random-int]]
   [transport.rhythm :only [select-mm]]))

(def min-segment-len 10000)  ;minimum segment length in milliseconds (10 seconds)
(def max-segment-len 30000)  ;maximum segment length in milliseconds (30 seconds)

(defn select-segment-length []
  (random-int min-segment-len max-segment-len))

(defn new-segment [player]
  (assoc player
         :instrument-info (select-instrument player),
         :key (select-key player),
         :melody [],
         :mm (select-mm player),
         :num-notes 10,
         :seg-len (select-segment-length),
         :seg-start 0,
         :scale (select-scale player)))
