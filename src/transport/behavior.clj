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

(ns transport.behavior
  (:use
   [overtone.live :only [ranged-rand]]
   [transport.players :only [PLAYERS get-behavior get-behavior-recording inc-behavior-recording rand-player-id-excluding-player set-behavior-player-id update-player-callback]]
   [transport.settings :only [NUM-PLAYERS COMPLEMENT CONTRAST FOLLOW IGNORE]]
   ))

(defn get-behavior-action
  [player]
  (:action (:behavior player)))

(defn set-behavior-recording
  "Will send-off a call to inc-behavior-recording
   this is to make certain that any pending operations
   on the player have been completed"
  [player-id]
  (send-off PLAYERS inc-behavior-recording player-id)
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

(defn select-and-set-behavior-player-id
  ;; if :behavior :action is not IGNORE
  ;;   returns a :behavior map with :player-id selected from PLAYERS
  ;; if :behavior is IGNORE returns the current :behavior map
  ;;
  ;; player - the player to set :behavior :player-id
  [player]
  (if (not= (:action (get-behavior player)) IGNORE)
    (let [player-id (rand-player-id-excluding-player player)]
      (set-behavior-player-id player player-id)
      )
    (get-behavior player)
    ))

(defn select-behavior
  [player]
  (let [action (if (> @NUM-PLAYERS 1) (select-behavior-action player) IGNORE)
        cur-recording (get-behavior-recording player)  ;; remember the number of players watching your recording
        ]
    {:accuracy (ranged-rand 0.25 0.85)
     :action action
     :recording (if (= cur-recording nil) 0 cur-recording)
     :player-id (rand-player-id-excluding-player player)
     })
  )
