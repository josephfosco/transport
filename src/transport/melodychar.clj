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

(defrecord MelodyChar [continuity note-durs range smoothness])

(defn get-melody-char-continuity
  [melody-char]
  (:continuity melody-char))

(defn get-melody-char-note-durs
  [melody-char]
  (:note-durs melody-char))

(defn get-melody-char-range
  [melody-char]
  (:range melody-char))

(defn get-melody-char-range-lo
  [melody-char]
  (first (:range melody-char)))

(defn get-melody-char-range-hi
  [melody-char]
  (second (:range melody-char)))

(defn get-melody-char-smoothness
  [melody-char]
  (:smoothness melody-char))

(defn set-melody-char-continuity
  [melody-char new-continuity]
  (assoc melody-char :continuity new-continuity))

(defn set-melody-char-note-durs
  [melody-char new-note-durs]
  (assoc melody-char :note-durs new-note-durs))

(defn set-melody-char-range
  [melody-char new-range]
  (assoc melody-char :range new-range))
