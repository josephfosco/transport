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

(ns transport.instruments.pitched-perc-instruments
  ^{:author "Joseph Fosco"
    :doc "Pitched Percussion Instruments"}
  (:require
   [overtone.live :refer :all]
   ))

(definst steel-drum
  [freq 440 amp 0.8 gate 0 action NO-ACTION]
  (-> (sin-osc (/ freq 2))
      (+ (rlpf (saw freq) (* 1.1 freq) 0.4))
      (* (env-gen (perc 0.01 0.5) gate (* amp 0.666) 0 1 action))
   )
  )

(definst steel-drum-sus
  [freq 440 amp 0.8]
  (-> (sin-osc (/ freq 2))
      (+ (rlpf (saw freq) (* 1.1 freq) 0.4))
      (* (env-gen (perc 0.01 0.5) 1 1 0 1 :action FREE))
      (* (/ amp 1.5))
   )
  )
(def steel-drum-sus-delay (inst-fx! steel-drum-sus fx-echo))
(ctl steel-drum-sus-delay :delay-time 0.1)
