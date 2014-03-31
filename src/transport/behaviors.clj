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

(ns transport.behaviors
  (:require
   [overtone.live :refer [ranged-rand]]
   [transport.behavior]
   [transport.players :refer [get-behavior rand-player-id-excluding-player set-behavior-player-id]]
   [transport.settings :refer [NUM-PLAYERS COMPLEMENT CONTRAST FOLLOW IGNORE]]
   )
  (:import transport.behavior.Behavior)
  )
(defn get-behavior-action-for-player
  [player]
  (:action (get-behavior player))
  )

(defn get-behavior-player-id-for-player
  [player]
  (:player-id (get-behavior player))
  )

(defn select-behavior-action
  [player]
  (let [action-num (rand)]
    (cond
     (< action-num 0.25) FOLLOW
     (< action-num 0.50) CONTRAST
     (< action-num 0.75) COMPLEMENT
     :else IGNORE
     ))  )

(defn select-behavior-ensemble-action
  [player]
  (let [action-num (rand)]
    (cond
     (< action-num 0.34) COMPLEMENT
     (< action-num 0.66) CONTRAST
     :else IGNORE
     ))  )

(defn select-and-set-behavior-player-id
  "If :behavior :action is not IGNORE
   returns a :behavior map with :player-id selected from PLAYERS
   if :behavior is IGNORE returns the current :behavior map

   player - the player to set :behavior :player-id"
  [player]
  (if (not= (:action (get-behavior player)) IGNORE)
    (let [player-id (rand-player-id-excluding-player player)]
      (set-behavior-player-id player player-id)
      )
    (get-behavior player)
    ))

(defn select-behavior
  [player]
  (let [behavior-action (if (> @NUM-PLAYERS 1) (select-behavior-action player) IGNORE)
        ;; select ensemble-action behavior only if not watching another player
        ensemble-action (if (and (= behavior-action IGNORE) (> @NUM-PLAYERS 1))
                          (select-behavior-ensemble-action player)
                          IGNORE)
        ]
    (Behavior. (ranged-rand 0.25 0.85)  ;; accuracy
               behavior-action          ;; action
               ensemble-action          ;; ensemble-action
               (if (not= behavior-action IGNORE) (rand-player-id-excluding-player player) nil)) ;; behavior player-id
    )
  )
