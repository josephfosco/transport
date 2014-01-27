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

(defn weighted-choice
  "Makes a random selection based on a vector of weights.
   Returns the index into the vector of the selection (0 - (length of vector - 1)

   weight-vector - vector of the form [x1 x2 x3 x4 ....]
                   where each entry si the relative weight of that entry"
  [weight-vector]
  (loop [i 0 rnd-num (* (rand) (reduce + weight-vector)) w-vec weight-vector]
    (let [rnd-total (- rnd-num (first w-vec))]
      (if (<= rnd-total 0)
        i
        (recur (inc i) rnd-total (rest w-vec))))))
