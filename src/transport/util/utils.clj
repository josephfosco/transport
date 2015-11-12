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

(ns transport.util.utils)

(defn average
 "Returns the average of a set of numbers"
 [nmbrs & {:keys [list-length] :or {list-length (count nmbrs)}}]
 (/ (apply + nmbrs) list-length))

(defmacro round-number
  [nmbr]
  `(if (> ~nmbr 0) (int (+ ~nmbr 0.5)) (int (- ~nmbr 0.5)))
  )

(defn get-percent
  "returns a percent of a number
   pct - the percent to return
   num - the number that is 100%"
  [pct num]
  (* num (/ pct 100))
  )

(defn nil-to-num
  "If val is not nil, returns val
   If val is nil returns num

   val - value to check
   num - num to return if val is nil"
  [val num]
  (if val val num)
  )

(defn get-max-map-key
  "For hash-maps with numeric keys, returns the highest key.
   Returns 0 if map is empty."
  [map]
  (reduce max 0 (keys map))
  )

(defn get-vector-max-frequency
  "Returns the non-nil value that occurs most often in the vector.
   If all values are unique, returns one of the values."
  [vec]
  (let [;; map of vector value and frequency with all nils removed
        vec-frequencies (dissoc (frequencies vec) nil)
        ;; most-used-val is a vector containing the value most used and the number of times it occurs
        most-used-val (first (for [x vec-frequencies :when (= (get x 1) (apply max (vals vec-frequencies)))] x))
        ]
    (get most-used-val 0)
    ))

(defmacro if-debug
  [& body]
  `(when ~'DEBUG ~@body)
  )
