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

(ns transport.instruments.osc-instruments
  ^{:author "Joseph Fosco"
    :doc "All instruments that are basically just an oscillator with an envelope"}
  (:require
   [overtone.live :refer :all]
   ))

(definst triangle-wave
  [freq 440 vol 0.4 attack 0.001 sustain 0.1 release 0.4 action FREE]
  (* (env-gen (lin attack sustain release) 1 1 0 1 action)
     (lf-tri freq)
     vol))

(definst tri-wave-sus
  [freq 440 vol 1.0 release 0.1 attack 0.01 sustain 0.3 gate 0.0 action FREE]
  (-> (lf-tri freq)
      (* (env-gen (asr attack sustain release) gate vol 0 1 action) )
      (* vol)
      )
  )

(definst saw-wave-sus
  [freq 440 vol 1.0 release 0.1 attack 0.01 sustain 0.3 gate 0.0 action FREE]
  (-> (lf-saw freq)
      (* (env-gen (asr attack sustain release) gate vol 0 1 action) )
      (* vol)
      )
  )

(definst sine-wave-sus
  [freq 440 vol 1.0 release 0.1 attack 0.01 sustain 0.3 gate 0.0 action FREE]
  (-> (sin-osc freq)
      (* (env-gen (asr attack sustain release) gate vol 0 1 action) )
      (* vol)
      )
  )

(definst saw-wave-timed
  [freq 440 gate-dur 0.8 vol 1.0 attack 0.01 sustain 0.3 release 0.1 action FREE]
  (let [env-gate (trig 1 gate-dur)
        ]
    (-> (lf-saw freq)
        (* (env-gen (asr attack sustain release) :gate env-gate :action action) )
        (* vol)
        )
    ))
