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

(ns transport.ensemble-status
  (:require
   [transport.messages :refer :all]
   [transport.message-processor :refer [register-listener]]
   [transport.players :refer :all]
   [transport.settings :refer :all]
   [transport.util :refer :all])
   )

(def note-values-millis (atom '(0 0 0 0 0 0 0 0 0 0)))
;; player-volumes is vector of the volume of the last not played for each player
;;  player-id is index into vector.
(def player-volumes (atom (apply vector (repeat @NUM-PLAYERS 0))))
(def rest-prob-len (* @NUM-PLAYERS 3))
;; rest-prob is list of true for notes, false for rests
(def rest-prob (atom '()))

(defn update-ensemble-status
  [& {:keys [player]}]
  (let [last-melody (get-last-melody-event player)]
    ;; if note (not rest) update note-values-millis with latest note rhythm value
    ;;   note-volumes with new note volume
    ;;   and rest-prob (with new note)
    ;; else just update rest-prob (with new rest) and note-volumes
    (do
      (reset! player-volumes (assoc @player-volumes (get-player-id player) (get-volume-for-note last-melody)))
      (if (not (nil? (get-note last-melody)))
        (do
          (reset! note-values-millis (conj (butlast @note-values-millis) (get-dur-millis (get-dur-info last-melody))))
          (reset! rest-prob (conj (butlast @rest-prob) true))
          )
        (do
          (reset! rest-prob (conj (butlast @rest-prob) false))
          )
        ))
    )
  )

(defn init-ensemble-status
  []
  (reset! note-values-millis '(0 0 0 0 0 0 0 0 0 0))
  (reset! player-volumes (apply vector (repeat @NUM-PLAYERS 0)))
  ;; initialize rest-prob
  (reset! rest-prob '())
  (dotimes [n rest-prob-len]
    (if (< (rand) 0.8)
      (reset! rest-prob (conj @rest-prob true))
      (reset! rest-prob (conj @rest-prob false))
      ))
  ;; update ensemble-status with each new note
  (register-listener
   MSG-PLAYER-NEW-NOTE
   transport.ensemble-status/update-ensemble-status
   {}
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
  (/ (reduce + @player-volumes) @NUM-PLAYERS))

(defn get-rest-probability
  "Compute the percent of rests in rest-prob returns fraction or float."
  []
  (/ (count (filter #(= false %1) @rest-prob)) rest-prob-len))
