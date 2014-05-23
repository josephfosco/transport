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
   [transport.message-processor :refer [register-listener send-message]]
   [transport.players :refer :all]
   [transport.settings :refer :all]
   [transport.util :refer :all])
   )

(def note-values-millis (atom '(0 0 0 0 0 0 0 0 0 0)))
;; player-volumes is vector of the volume of the last not played for each player
;;  player-id is index into vector.
(def player-volumes (atom (apply vector (repeat @NUM-PLAYERS 0))))
(def player-keys (atom (apply vector (repeat @NUM-PLAYERS (rand 12)))))
(def rest-prob-len (* @NUM-PLAYERS 3))
;; rest-prob is list of true for notes, false for rests
(def rest-prob (atom '()))

(defn- players-soft?
  "Returns true if the current volume of 90% of all players except
   exception-player-id is less than .25"
  [exception-player-id]
  (loop [rslt '() vols-to-check (assoc @player-volumes exception-player-id 0)]
    (cond (> (count rslt) (* @NUM-PLAYERS 0.1)) false
          (empty? vols-to-check) true
          (>= (first vols-to-check) 0.25) (recur (conj rslt true) (rest vols-to-check))
          :else (recur rslt (rest vols-to-check))
          )
    )
  )

(defn- send-status-msgs
  [player player-last-melody player-id note-time]
  (if (and (> (get-volume-for-note player-last-melody) 0.85)
           (> (get-dur-millis-for-note player-last-melody) 2000)
           (players-soft? player-id))
    (do
      (println "send-status-msgs volume:" (get-volume-for-note player-last-melody))
      (println "send-status-msgs dur-millis:" (get-dur-millis-for-note player-last-melody))
      (println "send-status-msgs players-volumes:" @player-volumes)

      (send-message MSG-LOUD-INTERUPT-EVENT :player-id player-id :time note-time)
      (println "ensemble-status.clj send-status-msgs - SENDING LOUD-INTERRUPT-EVENT MSG")
      )
    )
  )

(defn update-ensemble-status
  [& {:keys [player note-time]}]
  (let [last-melody (get-last-melody-event player)
        player-id (get-player-id player)
        ]
    ;; update this player's volume in player-volumes
    (reset! player-volumes (assoc @player-volumes player-id (get-volume-for-note last-melody)))
    ;; if player has new key - record it in player-keys
    (if (not= (get-key player) (get player-keys player-id))
      (reset! player-keys (assoc @player-keys player-id (get-key player)))
      )
    ;; if note (not rest) update note-values-millis with latest note rhythm value
    ;;   and rest-prob (with new note)
    ;; else just update rest-prob (with new rest) and note-volumes
    (if (not (nil? (get-note last-melody)))
      (do
        (reset! note-values-millis (conj (butlast @note-values-millis) (get-dur-millis (get-dur-info last-melody))))
        (reset! rest-prob (conj (butlast @rest-prob) true))
        )
      (do
        (reset! rest-prob (conj (butlast @rest-prob) false))
        )
      )
    (send-status-msgs player last-melody player-id note-time)
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

(defn get-ensemble-key-for-player
  "Select a kay for player from keys currently playing in ensemble"
  [player]
  (let [rand-index (rand-int (dec @NUM-PLAYERS)) ;; select a rand index into player-keys
        ]
    (if (>= rand-index (get-player-id player))   ;; return a key from player-keys but
      (get @player-keys (inc rand-index))         ;;  do not return key for player
      (get @player-keys rand-index))
    )
  )
