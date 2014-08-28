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

(ns transport.instruments.trad-instruments
  ^{:author "Joseph Fosco"
    :doc "Electronic imitations of traditional instruments"}
  (:require
   [overtone.live :refer :all]
   ))

(definst bassoon
  [freq 110 gate-dur 0.8 vol 1.0 attack 0.01 sustain 0.3 release 0.1]
  (let [env-gate (trig 1 gate-dur)
        ]
    (* (env-gen (asr attack sustain release) env-gate (* vol 4) 0 1 FREE)
       (bpf (saw freq) (* 2 freq) 2.0)))
  )
