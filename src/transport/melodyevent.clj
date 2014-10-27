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

(defrecord MelodyEvent [note dur-info follow-note instrument-info volume seg-num])

(defn create-melody-event
  [& {:keys [note dur-info follow-note instrument-info volume seg-num]}]
  (MelodyEvent. note
                dur-info
                follow-note
                instrument-info
                volume
                seg-num
                )
  )

(defn get-dur-info-for-event
  [melody-event]
  (:dur-info melody-event))

(defn get-follow-note-for-event
  [melody-event]
  (:follow-note melody-event))

(defn get-instrument-info-for-event
  [melody-event]
  (:instrument-info melody-event))

(defn get-note-for-event
  [melody-event]
  (:note melody-event))

(defn get-seg-num-for-event
  [melody-event]
  (:seg-num melody-event))

(defn get-volume-for-event
  [melody-event]
  (:volume melody-event))
