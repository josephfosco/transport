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
   [transport.settings :only [NUM-PLAYERS]]
   [transport.players :only [PLAYERS rand-player-id-excluding-player]]
   ))

(def FOLLOW 0)
(def CONTRAST 1)
(def COMPLEMENT 2)
(def IGNORE 3)

(defn get-behavior
  [player]
  (:behavior player))

(defn get-behavior-action
  [player]
  (:action (:behavior player)))

(defn get-behavior-player-id
  [player]
  (:player-id (:behavior player)))

(defn set-behavior-player-id
  [player player-id]
  (assoc (:behavior player)
    :player-id player-id)
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
  (if (not= (:action (:behavior player)) IGNORE)
    (let [player-id (rand-player-id-excluding-player player)]
      (set-behavior-player-id player player-id)
      )
    (:behavior player)
    ))

(defn select-behavior
  [player]
  (let [action (if (> NUM-PLAYERS 1) (select-behavior-action player) IGNORE)]
    {:accuracy (ranged-rand 0.25 0.85)
     :action action
     :player-id (rand-player-id-excluding-player player)
     })
  )
