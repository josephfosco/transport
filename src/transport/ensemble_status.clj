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
   [transport.players :only [get-dur-info get-dur-millis get-last-melody-event get-note]]
   [transport.util])
   )

(def note-values-millis (atom '(0 0 0 0 0 0 0 0 0 0)))

(defn update-ensemble-status
  [player]
  (let [last-melody (get-last-melody-event player)]
    ;; update note-values-millis with latest note rhythm value
    (if (not (nil? (get-note last-melody)))
      (reset! note-values-millis (conj (butlast @note-values-millis) (get-dur-millis (get-dur-info last-melody)))))
    )
  (println "note-values-millis: " note-values-millis)
  )

(defn new-note-value
  [new-val]
  (reset! note-values-millis (conj (butlast @note-values-millis) new-val))
  )

(defn reset-ensemble-status
  []
  (reset! note-values-millis '(0 0 0 0 0 0 0 0 0 0)))

(defn get-average-note-val-millis
  []
  (/ (reduce + @note-values-millis) (count @note-values-millis)))
