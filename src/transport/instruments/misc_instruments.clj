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

(ns transport.instruments.misc-instruments
  ^{:doc "Miscellaneous Instruments collected from various sources"}
  (:require
   [overtone.live :refer :all]
   ))


(definst bass-m1
  [freq 440  dur 1000 vol 0.4]
  (-> (saw freq)
      (rlpf (line:kr 2000 (/ dur 1000.0) freq) 0.5)
      (* ( env-gen (perc 0.1 (/ (max (- dur 0.1) 0.05) 1000.0)) :action FREE))
      (* vol)
      )
  )

(definst organ-m1
  [freq 440 dur 1000 vol 0.4 land 0.9]
  (-> (square freq)
      (+ (sin-osc (* 3 freq) (sin-osc 6)))
      (+ (sin-osc (* 1/2 freq) (sin-osc 3)))
      (* (env-gen (adsr 0.03 0.3 0.4 0.4) (line:kr 1 0 (/ (max (- dur 0.4) 0.05) 1000.0)) :action FREE))
      (* (sin-osc (* 2 freq)))
      (clip2 (line:kr 1 land 16))
      (* (/ vol 1.5))
      )
  )

(definst drum-m1
  [freq 440 vol 0.4]
  (-> (line:kr freq (* freq 1/2) 0.5)
      sin-osc
      (+ (sin-osc freq))
      (+ (sin-osc (/ freq 2) (sin-osc 1)))
      (* (env-gen (perc 0.01 0.1) :action FREE))
      (* (/ vol 1.5))
      )
  )

(definst plink-m1
  [freq 440 vol 0.4]
  (-> (sin-osc freq)
      (+ (* 1/3 (sin-osc (* freq 3))))
      (+ (* 1/5 (sin-osc (* freq 5.1))))
      (+ (* 1/6 (sin-osc (* freq 6.1))))
      (+ (* 1/8 (sin-osc (* freq 7.1))))
      (+ (* 1/8 (sin-osc (* freq 8))))
      (* (env-gen (perc 0.01 0.3) :action FREE))
      (* (/ vol 9))
      )
  )
