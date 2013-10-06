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

(ns transport.instruments.osc-instruments
  ^{:author "Joseph Fosco"
    :doc "All instruments that are basically just an oscillator with an envelope"}
  (:use
   [overtone.live]
   ))

(definst triangle-wave
  [freq 440 attack 0.01 sustain 0.1 release 0.4 vol 0.4]
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (lf-tri freq)
     vol))

(definst tri-wave-sus
  [freq 440 gate-dur 0.8 attack 0.01 sustain 0.3 release 0.1 vol 0.4]
  (let [env-gate (trig 1 gate-dur)
        ]
    (* (env-gen (asr attack sustain release) env-gate 1 0 1 FREE)
       (lf-tri freq))))

(definst saw-wave-sus
  [freq 440 gate-dur 0.8 attack 0.01 sustain 0.3 release 0.1 vol 0.4]
  (let [env-gate (trig 1 gate-dur)
        ]
    (* (env-gen (asr attack sustain release) env-gate 1 0 1 FREE)
       (lf-saw freq))))

(definst sine-wave-sus
  [freq 440 gate-dur 0.8 attack 0.01 sustain 0.3 release 0.1 vol 0.4]
  (let [env-gate (trig 1 gate-dur)
        ]
    (* (env-gen (asr attack sustain release) env-gate 1 0 1 FREE)
       (sin-osc freq))))
