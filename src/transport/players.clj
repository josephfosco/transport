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
  (vals @PLAYERS))

(defn get-player
  [player-id]
  (get @PLAYERS player-id))

(defn get-melody
  [player]
  (:melody player))

(defn get-last-melody-event
  [player]
  (let [player-melody (get-melody player)]
    (if (= player-melody {}) nil (get player-melody (reduce max 0 (keys player-melody))))
    )
  )

(defn get-note
  [melody-event]
  (:note melody-event))

(defn get-dur-info
  [melody-event]
  (:dur-info melody-event))

(defn get-dur-millis
  "Returns the millis duraition for the dur-info

   dur-info - duration info to get millis from"
  [dur-info]
  (:dur-millis dur-info)
  )

(defn get-behavior
  [player]
  (:behavior player))

(defn get-behavior-action
  [player]
  (:action (get-behavior player)))

(defn get-behavior-ensemble-action
  [player]
  (:ensemble-action (get-behavior player)))

(defn get-behavior-player-id
  [player]
  (:player-id (:behavior player)))

(defn get-mm
  [player]
  (:mm player))

(defn get-player-id
  [player]
  (:player-id player))

(defn get-last-melody-note
  "Returns the last melody note played by player.
   Retunns nil if last note was a rest

  player - the player to get the melody note from"
  [player]
  (let [cur-melody (get-melody player)]
    (if (= cur-melody {})
      nil
      (:note (get (reduce max (keys cur-melody)) cur-melody))))
  )

(defn clear-players
  "used by send or send-off to clear agents"
  [cur-players]
  {}
  )

(defn reset-players
  []
  (send-off PLAYERS clear-players)
  (await PLAYERS))

(defn set-behavior
  [player behavior]
  (assoc player :behavior behavior)
  )

(defn set-behavior-player-id
  "Returns new :behavior map with :player-id set to player-id

   player - the player whose :behavior is to be changed
   player-id - the player-id to set :player-id to"
  [player player-id]
  (assoc (:behavior player)
    :player-id player-id)
  )

(defn update-player-callback
  "update the value of a player in agent PLAYERS
   this is called from send-off"
  [cur-players new-player]
  (let [player-id (get-player-id new-player)
        old-behavior-player-id (get-behavior-player-id (get cur-players player-id))
        new-behavior-player-id (get-behavior-player-id new-player)
        ]
    (assoc @PLAYERS player-id new-player)
    ))

(defn update-player
  [player]
  (send-off PLAYERS update-player-callback player))

(defn rand-player-id-excluding-player
  "Select a random player-id not including the
   player-id of player

   player - player to exclude fromm possible player-ids"
  [player]
  (if (> (count @PLAYERS) 0)
    (rand-nth (keys (dissoc @PLAYERS (:player-id player))))
    nil
    ))
