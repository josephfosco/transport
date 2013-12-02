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

(defn get-player-id
  [player]
  (:player-id player))

(defn clear-players
  "used by send or send-off to clear agents"
  [cur-players]
  {}
  )

(defn reset-players
  []
  (send-off PLAYERS clear-players)
  (await PLAYERS))

(defn get-behavior
  [player]
  (:behavior player))

(defn set-behavior
  [player behavior]
  (assoc player :behavior behavior)
  )

(defn get-behavior-recording
  [player]
  (:recording (:behavior player)))

(defn get-behavior-player-id
  [player]
  (:player-id (:behavior player)))

(defn set-behavior-player-id
  "Returns new :behavior map with :player-id set to player-id

   player - the player whose :behavior is to be changed
   player-id - the player-id to set :player-id to"
  [player player-id]
  (assoc (:behavior player)
    :player-id player-id)
  )

(declare update-player-callback)
(defn inc-behavior-recording
  "Called from send-off. Will increment the current value
   of :recording.

   cur-players - the current PLAYERS map
   player-id - the player-id to increment :recording"
  [cur-players player-id]

  (let [player get cur-players player-id]
    (update-player-callback cur-players
                   (set-behavior (assoc (get-behavior player) :recording (+ (get-behavior-recording player) 1))))
    ))

(defn update-player-callback
  "update the value of a player in agent PLAYERS
   this is called from send-off"
  [cur-players new-player]
  (let [player-id (get-player-id new-player)
        old-behavior-player-id (get-behavior-player-id (get cur-players player-id))
        new-behavior-player-id (get-behavior-player-id new-player)
        ]
    (assoc @PLAYERS player-id new-player)
    ;;(println "player-id old new:" player-id old-behavior-player-id new-behavior-player-id)
    (comment
      (if (not=
           old-behavior-player-id
           new-behavior-player-id)
        (if (not= (get-behavior-player-id new-player) nil)
          (do
            ;; (inc-behavior-recording cur-players (get-behavior-player-id new-player))
            (println "inc recording player-id: " (get-behavior-player-id new-player)))
          (println "player-id is nil"))
        ))
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
