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

(ns transport.random)

(def SAME 0)
(def CONTRAST 1)
(def IGNORE 2)

(defn random-int
  "Returns a random integer between lo (inclusive) and hi (inclusive).
  Does not check that lo < hi"
  [lo hi]
  (+ (rand-int (inc (- hi lo)))lo))

(defn random-pitch
  [lo-note hi-note]
  (random-int lo-note hi-note))

(defn random-dur
  [lo-millis hi-millis]
  (random-int lo-millis hi-millis))

(defn same-contranst-ignore
  []
  (let [sci-num (rand)]
    (cond
     (< sci-num 0.33) SAME
     (< sci-num 0.66) CONTRAST
     :else IGNORE
     )))
