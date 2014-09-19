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
   [transport.behavior :refer [get-behavior-action get-behavior-player-id]]
   [transport.players :refer [get-behavior rand-player-id-excluding-player set-behavior-player-id]]
   [transport.settings :refer :all]
   )
  (:import transport.behavior.Behavior)
  )
(defn get-behavior-action-for-player
  [player]
  (get-behavior-action (get-behavior player))
  )


(defn get-behavior-player-id-for-player
  [player]
  (get-behavior-player-id (get-behavior player))
  )

(defn select-behavior-action
  [player]
  (let [action-num (rand)]
    (cond
     (< action-num 0.17) FOLLOW-PLAYER
     (< action-num 0.34) CONTRAST-PLAYER
     (< action-num 0.51) SIMILAR-PLAYER
     (< action-num 0.68) SIMILAR-ENSEMBLE
     (< action-num 0.85) CONTRAST-ENSEMBLE
     :else IGNORE
     ))  )

(defn select-behavior-ensemble-action
  [player]
  (let [action-num (rand)]
    (cond
     (< action-num 0.34) SIMILAR-ENSEMBLE
     (< action-num 0.66) CONTRAST-ENSEMBLE
     :else IGNORE
     ))  )

(defn select-and-set-behavior-player-id
  "If :behavior :action is not IGNORE
   returns a :behavior map with :player-id selected from PLAYERS
   if :behavior is IGNORE returns the current :behavior map

   player - the player to set :behavior :player-id"
  [player]
  (if (not= (get-behavior-action (get-behavior player)) IGNORE)
    (let [player-id (rand-player-id-excluding-player player)]
      (set-behavior-player-id player player-id)
      )
    (get-behavior player)
    ))

(defn select-first-behavior
  "Only used the first time Behavior is set.
   Always sets behavior player-id to nil.

   player - the player to set behavior for"
  [player]
  (let [behavior-action (if (> @number-of-players 1) (select-behavior-action player) IGNORE)
        ;; select ensemble-action behavior only if not watching another player
        ]
    (Behavior. (ranged-rand 0.25 0.85)  ;; accuracy
               behavior-action          ;; action
               nil)                     ;; behavior player-id
    )
  )

(defn select-behavior
  [player]
  (let [behavior-action (if (> @number-of-players 1) (select-behavior-action player) IGNORE)
        ;; select ensemble-action behavior only if not watching another player
        ]
    (Behavior. (ranged-rand 0.25 0.85)  ;; accuracy
               behavior-action          ;; action
               (if (not= behavior-action IGNORE) (rand-player-id-excluding-player player) nil)) ;; behavior player-id
    )
  )
