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
   [transport.players :only [get-dur-info get-dur-millis get-last-melody-event get-note get-volume-for-note]]
   [transport.settings]
   [transport.util])
   )

(def note-values-millis (atom '(0 0 0 0 0 0 0 0 0 0)))
(def note-volumes-len (* @NUM-PLAYERS 3))
;; note-volumes is list of volumes for notes
(def note-volumes (atom '()))
(def rest-prob-len (* @NUM-PLAYERS 3))
;; rest-prob is list of true for notes, false for rests
(def rest-prob (atom '()))

(defn init-ensemble-status
  []
  (reset! note-values-millis '(0 0 0 0 0 0 0 0 0 0))
  ;; initialize rest-prob
  (reset! rest-prob '())
  (dotimes [n rest-prob-len]
    (if (< (rand) 0.8)
      (reset! rest-prob (conj @rest-prob true))
      (reset! rest-prob (conj @rest-prob false))
      ))
  ;; initialize note-volumes to random values
  (reset! note-volumes '())
  (dotimes [n note-volumes-len]
    (reset! note-volumes (conj @note-volumes (rand)))
    )
  )

(defn update-ensemble-status
  [player]
  (let [last-melody (get-last-melody-event player)]
    ;; if note (not rest) update note-values-millis with latest note rhythm value
    ;;   and rest-prob (with new note)
    ;; else just update rest-prob (with new rest)
    (if (not (nil? (get-note last-melody)))
      (do
        (reset! note-values-millis (conj (butlast @note-values-millis) (get-dur-millis (get-dur-info last-melody))))
        (reset! note-volumes (conj (butlast @note-volumes) (get-volume-for-note last-melody)))
        (reset! rest-prob (conj (butlast @rest-prob) true))
        )
      (do
        (reset! note-volumes (conj (butlast @note-volumes) (get-volume-for-note last-melody)))
        (reset! rest-prob (conj (butlast @rest-prob) false))
        )
      )
    )
  )

(defn reset-ensemble-status
  []
  (init-ensemble-status)
  )

(defn get-average-note-dur-millis
  []
  (/ (reduce + @note-values-millis) (count @note-values-millis)))

(defn get-average-volume
  []
  (/ (reduce + @note-volumes) note-volumes-len))

(defn get-rest-probability
  "Compute the percent of rests in rest-prob returns fraction or float."
  []
  (/ (count (filter #(= false %1) @rest-prob)) rest-prob-len))
