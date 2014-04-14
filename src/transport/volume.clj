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

(ns transport.volume
  (:require
   [transport.settings :refer [COMPLEMENT FOLLOW]]
   ))

(defn select-volume
  [player]
  (rand))

(defn select-volume-in-range
  "Returns a random float between lo-vol and hi-vol(exclusive).

  lo-vol - lowest value that can be returned
  hi-vol - highest value that can be returned(exclusive)"
  [lo-vol hi-vol]
  (+ (rand (- hi-vol lo-vol)) lo-vol))
