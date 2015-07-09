;    Copyright (C) 2013-2015  Joseph Fosco. All Rights Reserved
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

(defn set-number-of-players
  [new-num-players]
  (reset! number-of-players new-num-players))

(def ensemble-mm-change-threshold (atom 5))
(def ensemble-volume-change-threshold (atom 0.03))
(def ensemble-density-change-threshold (atom 0.08))
(def ensemble-pitch-change-threshold (atom 12))

(def min-volume 0.2)
