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

(ns transport.melodychar)

(defrecord MelodyChar [continuity density range smoothness])

(defn get-melody-char-continuity
  [melody-char]
  (:continuity melody-char))

(defn get-melody-char-density
  [melody-char]
  (:density melody-char))

(defn get-melody-char-range
  [melody-char]
  (:range melody-char))

(defn get-melody-char-range-hi
  [melody-char]
  (first (:range melody-char)))

(defn get-melody-char-range-lo
  [melody-char]
  (second (:range melody-char)))

(defn get-melody-char-smoothness
  [melody-char]
  (:smoothness melody-char))
