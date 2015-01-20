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

(ns transport.instruments.elec-instruments
  ^{:author "Joseph Fosco"
    :doc "Electronic instruments"}
  (:require
   [overtone.live :refer :all]
   ))

(definst reedy-organ
  [freq 440 vol 0.3 release 0.1 attack 0.01 sustain 0.3 gate 1.0 action FREE]
  (-> (sin-osc freq)
      (+ (saw freq) (saw (+ freq 3)) (sin-osc (* 2 freq)))
      (* (env-gen (asr attack sustain release) gate vol 0 1 action))
      (* vol)
      )
  )
