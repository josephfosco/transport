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

(ns transport.players
  (:require
   [transport.behavior]
   [transport.message_processor :refer [send-message register-listener]]
   [transport.messages :refer :all]
   [transport.settings :refer :all]
   )
  (:import transport.behavior.Behavior)
  )

(def PLAYERS (agent {}))

(defn get-players
  []
  (vals @PLAYERS))

(defn get-player
  [player-id]
  (get @PLAYERS player-id))

(defn get-note
  [melody-event]
  (:note melody-event))

(defn get-behavior
  [player]
  (:behavior player))

(defn get-behavior-ensemble-action
  [player]

  (:ensemble-action (get-behavior player)))

(defn get-dur-info
  [melody-event]
  (:dur-info melody-event))

(defn get-dur-millis
  "Returns the millis duraition for the dur-info

   dur-info - duration info to get millis from"
  [dur-info]
  (:dur-millis dur-info)
  )

(defn get-function
  [player]
  (:function player))

(defn get-instrument-info
  [player]
  (:instrument-info player))

(defn get-key
  [player]
  (:key player))

(defn get-melody-char
  [player]
  (:melody-char player))

(defn get-melody-continuity-char
  [player]
  (:continuity (:melody-char player)))

(defn get-melody-density-char
  [player]
  (:density (:melody-char player)))

(defn get-melody-range-char
  [player]
  (:range (:melody-char player)))

(defn get-melody-smoothness-char
  [player]
  (:smoothness (:melody-char player)))

(defn get-metronome
  [player]
  (:metronome player))

(defn get-mm
  [player]
  (:mm player))

(defn get-player-id
  [player]
  (:player-id player))

(defn get-melody
  [player]
  (:melody player))

(defn get-last-melody-event-num
  [player-id]
  (let [last-melody-key (reduce max 0 (keys (get-melody (get-player player-id))))]
    (if (= last-melody-key 0) nil last-melody-key)
    )
  )

(defn get-last-melody-note
  "Returns the last melody note played by player.
   Retunns nil if last note was a rest

  player - the player to get the melody note from"
  [player]
  (let [cur-melody (get-melody player)]
    (if (= cur-melody {})
      nil
      (:note (get cur-melody (reduce max (keys cur-melody))))))
  )

(defn get-last-melody-event
  [player]
  (let [player-melody (get-melody player)]
    (if (= player-melody {}) nil (get player-melody (reduce max 0 (keys player-melody))))
    )
  )

(defn get-scale
  [player]
  (:scale player))

(defn get-volume-for-note
  [melody-event]
  (:volume melody-event))

(defn clear-players
  "used by send or send-off to clear agents"
  [cur-players]
  {}
  )

(defn reset-players
  []
  (send PLAYERS clear-players)
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
  (assoc @PLAYERS (get-player-id new-player) new-player)
  )

(defn update-player
  [player]
  (send PLAYERS update-player-callback player)
  (await PLAYERS)
  )

(defn rand-player-id-excluding-player
  "Select a random player-id not including the
   player-id of player

   player - player to exclude from possible player-ids"
  [player]
  (if (> (count @PLAYERS) 0)
    (rand-nth (keys (dissoc @PLAYERS (:player-id player))))
    nil
    ))

(defn get-complement-info-from-player
  "follow-player - the player to get the following info from"
  [follow-player]
  {
   :key (get-key follow-player)
   :melody-char (get-melody-char follow-player)
   :metronome (get-metronome follow-player)
   :mm (get-mm follow-player)
   :scale (get-scale follow-player)
   }
  )

(defn get-following-info-from-player
  "follow-player - the player to get the following info from"
  [follow-player]
  (assoc (get-complement-info-from-player follow-player)
   :instrument-info (get-instrument-info follow-player)
    )
  )

(defn copy-follow-complement-info
  [cur-players from-player-id to-player-id originator-player-id]
  (println "players - copy-follow-complement-info from:" from-player-id "to:" to-player-id "originator:" originator-player-id)
  (let [to-player (get-player to-player-id)]
    (if (= from-player-id (:player-id (:behavior to-player)))
      (do
        (if (not= originator-player-id to-player-id)
          (do
            (send-message MSG-PLAYER-NEW-FOLLOW-INFO :change-player-id to-player-id :originator-player-id  originator-player-id)
            (send-message MSG-PLAYER-NEW-COMPLEMENT-INFO :change-player-id to-player-id :originator-player-id  originator-player-id))
          (println "players.clj - copy-follow-complement-info - NOT SENDING MESSAGES"))
        (assoc @PLAYERS to-player-id
               (merge to-player
                      (if (= (:action (:behavior to-player)) FOLLOW)
                        (get-following-info-from-player (get-player from-player-id))
                        (get-complement-info-from-player (get-player from-player-id))
                        ))))
      (do
        (println "players - copy-follow-info NOT COPYING!")
        cur-players)))
  )

(defn- new-complement-info?
  [from-player to-player]
  (if (or (not= (get-key from-player) (get-key to-player))
          (not= (get-melody-char from-player) (get-melody-char to-player))
          (not= (get-metronome from-player) (get-metronome to-player))
          (not= (get-mm from-player) (get-mm to-player))
          (not= (get-scale from-player) (get-scale to-player))
          )
    true    ;; there is new complement info
    false   ;; there is NO new complement info
   )
  )

(defn- new-follow-info?
  [from-player to-player]
  (if (or (not= (get-instrument-info from-player) (get-instrument-info to-player))
          (new-complement-info? from-player to-player)
          )
    true    ;; there is new follow info
    false   ;; there is NO new follow info
   )
  )

(defn player-new-follow-info
  [& {:keys [change-player-id follow-player-id originator-player-id]}]
  (if (new-follow-info? (get-player change-player-id) (get-player follow-player-id))
    (send PLAYERS copy-follow-complement-info change-player-id follow-player-id  originator-player-id)
    (println "players.clj - player-new-follow-info: ***** NOT COPYING FOLLOW INFO *****")
    )
  )

(defn player-new-complement-info
  [& {:keys [change-player-id follow-player-id originator-player-id]}]
  (if (new-complement-info? (get-player change-player-id) (get-player follow-player-id))
    (send PLAYERS copy-follow-complement-info change-player-id follow-player-id originator-player-id)
    (println "players.clj - player-new-complement-info: ***** NOT COPYING COMPLEMENT INFO *****")
    )
  )

(defn init-players
  []
;;  (register-listener MSG-PLAYER-NEW-SEGMENT player-new-segment nil)
  )

(defn print-player
  "Pretty Print a player map

  player - the player map to print"
  [player & {:keys [prnt-full-inst-info]
             :or {prnt-full-inst-info false}}]
  (let [sorted-keys (sort (keys player))]
    (println "player:")
    (doseq [player-key sorted-keys]
      (if (and (= player-key :instrument-info) (= prnt-full-inst-info false))
        (do
          (println (format "%-29s" (str "  " player-key " :name")) "-" (:name (:instrument (:instrument-info player))))
          (println (format "%-29s" (str "  " player-key " :range-lo")) "-" (:range-lo (:instrument-info player)))
          (println (format "%-29s" (str "  " player-key " :range-hi")) "-" (:range-hi (:instrument-info player))))
        (println (format "%-20s" (str "  " player-key)) "-" (get player player-key)))
      )
    (prn)
    )
  )

(defn print-player-num
  [player-id]
  (print-player (get-player player-id))
  )

(defn print-player-long
  "Pretty Print a player map with all instrument-info

  player - the player map to print"
  [player]
  (print-player player :prnt-full-inst-info true)
  )

(defn print-all-players
  []
  (dorun (map print-player (get-players)))
)
