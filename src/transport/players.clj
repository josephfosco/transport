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

(ns transport.players)

(def PLAYERS (agent {}))

(defn get-players
  []
  @PLAYERS)

(defn get-player
  [player-id]
  (get @PLAYERS player-id))

(defn clear-players
  "used by send or send-off to clear agents"
  [cur-players]
  {}
  )

(defn reset-players
  []
  (send-off PLAYERS clear-players)
  (await PLAYERS))

(defn update-player
  "update the value of a player in agent PLAYERS
   this is called from send-off"
  [cur-players new-player]
  (assoc @PLAYERS (:player-id new-player) new-player)
  )

(defn rand-player-id-excluding-player
  "Select a random player-id not including the
   player-id of player

   player - player to exclude fromm possible player-ids"
  [player]
  (if (> (count @PLAYERS) 0)
    (rand-nth (keys (dissoc @PLAYERS (:player-id player))))
    nil
    ))
