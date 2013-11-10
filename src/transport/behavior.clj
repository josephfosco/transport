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
   [transport.players :only [PLAYERS]]
   [transport.random :only [follow-contrast-ignore FOLLOW CONTRAST IGNORE]]
   ))

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
(defn select-and-set-behavior-player-id
  [player]
  (let [player-id (rand-nth (keys (dissoc @PLAYERS (:player-id player))))]
    (set-behavior-player-id player player-id)
    ))

(defn select-behavior
  [player]
  (let [action (if (> NUM-PLAYERS 1) (follow-contrast-ignore) IGNORE)]
    {:accuracy (ranged-rand 0.25 0.85)
     :action action
     :player-id nil
     })
  )
