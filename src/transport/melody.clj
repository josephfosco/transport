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

(ns transport.melody
  (:use
   [transport.pitch :only [next-pitch]]
   [transport.random :only [random-int]]
   [transport.rhythm :only [next-note-dur]]))

(defn note-or-rest [note-prob]
  (let [play-note? (random-int 0 note-prob)]
    (if (pos? play-note?)
      true
      nil)))

(defn next-melody
  "Returns the next note and it's duration as a map
    player is the player map
    keys are :note :dur"
  [player]
  {:note (if (note-or-rest 8) (next-pitch player) nil)
   :dur (next-note-dur player)}
  )
