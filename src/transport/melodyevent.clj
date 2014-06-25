;    Copyright (C) 2014  Joseph Fosco. All Rights Reserved
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

(ns transport.melodyevent)

(defrecord MelodyEvent [note dur-info follow-note instrument-info volume])

(defn get-dur-info
  [melody-event]
  (:dur-info melody-event))

(defn get-dur-millis
  "Returns the millis duraition for the dur-info

   dur-info - duration info to get millis from"
  [dur-info]
  (:dur-millis dur-info)
  )

(defn get-follow-note
  [melody-event]
  (:follow-note melody-event))

(defn get-volume
  [melody-event]
  (:volume melody-event))
