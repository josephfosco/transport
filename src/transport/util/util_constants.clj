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

(ns transport.util.util-constants
  "This namespace is a 'terminal namespace'.
   It should not :require :use :refer or :import any
   other transport namespaces"
  (:require
   [overtone.live :refer [MIDI-RANGE]]
   )
  )

(def DECREASING 10)
(def STEADY 11)
(def INCREASING 12)
