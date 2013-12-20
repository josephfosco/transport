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

(ns transport.ensemble-status
  (:use
   [transport.util])
   )

(def rhythm-values-millis (atom '(0 0 0 0 0 0 0 0 0 0)))

(defn new-rhythm-value
  [new-val]
  (reset! rhythm-values-millis (conj (butlast @rhythm-values-millis) new-val))
  )

(defn reset-ensemble-status
  []
  (reset! rhythm-values-millis '(0 0 0 0 0 0 0 0 0 0)))

(defn get-average-rhythm-val-millis
  []
  (/ (reduce + @rhythm-values-millis) (count @rhythm-values-millis)))
