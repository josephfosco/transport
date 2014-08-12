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

(ns transport.players
  (:require
   [transport.behavior :refer [get-behavior-player-id]]
   [transport.melodyevent :refer [get-follow-note-for-event get-instrument-info-for-event]]
   [transport.message-processor :refer [send-message register-listener]]
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

(defn get-behavior
  [player]
  (:behavior player))

(defn get-change-follow-info-note
  [player]
  (:change-follow-info-note player))

(defn get-function
  [player]
  (:function player))

(declare get-last-melody-event)
(defn get-instrument-info
  "Returns instrument info if it is not nil else
     returns instrument info from last melody-event

   player - the player to return instrument info from"
  [player]
  (let [instrument-info (:instrument-info player)]
    (if (nil? instrument-info)
      (get-instrument-info-for-event (get-last-melody-event player))
      instrument-info
      )))

(defn get-key
  [player]
  (:key player))

(defn get-last-pitch
  [player]
  (:last-pitch player))

(defn get-melody
  [player]
  (:melody player))

(defn get-melody-char
  [player]
  (:melody-char player))

(defn get-metronome
  [player]
  (:metronome player))

(defn get-mm
  [player]
  (:mm player))

(defn get-player-id
  [player]
  (:player-id player))

(defn get-scale
  [player]
  (:scale player))

(defn get-seg-num
  [player]
  (:seg-num player))

(defn get-volume-for-note
  [melody-event]
  (:volume melody-event))

(defn get-dur-millis-for-note
  [melody-event]
  (:dur-millis (:dur-info melody-event)))

(defn get-last-melody-event-num
  [player-id]
  (let [last-melody-key (reduce max 0 (keys (get-melody (get-player player-id))))]
    (if (= last-melody-key 0) nil last-melody-key)
    )
  )

(defn get-last-melody-event-num-for-player
  [player]
  (let [last-melody-key (reduce max 0 (keys (get-melody player)))]
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
    :instrument-info nil
    :change-follow-info-note nil
    )
  )

(defn set-change-follow-info-note
  [cur-players from-player-id to-player-id originator-player-id melody-no]
  (println "players.clj - set-change-follow-info-note from:" from-player-id "to:" to-player-id "originator:" originator-player-id "melody-no:" melody-no)
  (if (not= originator-player-id to-player-id)
    (do
      (let [to-player (get-player to-player-id)]
        (if (= from-player-id (get-player-id (:behavior to-player)))
          (assoc @PLAYERS to-player-id
                 (assoc to-player :change-follow-info-note melody-no))
          (do
            (println "players.clj - set-change-follow-info-note NOT COPYING!")
            cur-players))))
    (do
      (println "players.clj - set-change-follow-info-note same originator NOT COPYING!")
      cur-players)

    )
  )

(defn- send-new-player-info-msgs
  [change-player-id originator-player-id melody-no]
  (send-message MSG-PLAYER-NEW-FOLLOW-INFO :change-player-id change-player-id :originator-player-id  originator-player-id :melody-no melody-no)
  (send-message MSG-PLAYER-NEW-COMPLEMENT-INFO :change-player-id change-player-id :originator-player-id  originator-player-id)
  (send-message MSG-PLAYER-NEW-CONTRAST-INFO :change-player-id change-player-id :originator-player-id  originator-player-id)

  )

(defn set-new-contrast-info
  [cur-players change-player-id contrasting-player-id originator-player-id new-contrasting-info-map]
  (println "players - set-new-contrast-info changing:" change-player-id "contrasting:" contrasting-player-id "originator:" originator-player-id)
  (let [contrasting-player (get-player contrasting-player-id)]
    (if (= change-player-id (get-player-id (:behavior contrasting-player)))
      (do
        (if (not= originator-player-id contrasting-player-id)
          (do
            (send-new-player-info-msgs contrasting-player-id originator-player-id (get-last-melody-event-num-for-player contrasting-player))
            (assoc @PLAYERS contrasting-player-id (merge contrasting-player new-contrasting-info-map))
            )
          (println "players.clj - set-new-contrast-info - NOT SENDING MESSAGES OR SETTING FOR CONTRAST"))
        cur-players)
      (do
        (println "players - set-new-contrast-info NOT SETTING CONTRAST!")
        cur-players)))
  )

(declare print-player)
(defn copy-follow-info
  [cur-players to-player]
  (let [to-player-id (get-player-id to-player)
        from-player-id (get-behavior-player-id (get-behavior to-player))
        cur-change-follow-info-note (get-change-follow-info-note to-player)
        last-follow-note (get-follow-note-for-event (get-last-melody-event to-player))
        ]
    (println "players - copy-follow-info to-player-id:" to-player-id)
    (if (and
         (not (nil? from-player-id))
         (not (nil? cur-change-follow-info-note))
         (not (nil? last-follow-note))
         (>= (inc last-follow-note) cur-change-follow-info-note))
      (do
        (send-new-player-info-msgs to-player-id to-player-id (get-last-melody-event-num-for-player to-player))
        (assoc @PLAYERS to-player-id
               (merge to-player
                      (get-following-info-from-player (get-player from-player-id))
                      )))
      (do
        (println "players.clj - copy-follow-info NOT COPYING FOLLOW INFO from-player-id:" from-player-id "to-player-id:" to-player-id "cur-change-follow-info-note:" cur-change-follow-info-note "last-follow-note:" last-follow-note)
        (print-player to-player)
        (assoc @PLAYERS to-player-id to-player)
        )
      )
    )
  )

(defn replace-complement-info
  [cur-players from-player-id to-player originator-player-id]
  (let [to-player-id (get-player-id to-player)]
    (println "players.clj - replace-complement-info from:" from-player-id "to:" to-player-id "originator:" originator-player-id)
    (if (= from-player-id (get-player-id (:behavior to-player)))
      (do
        (if (not= originator-player-id to-player-id)
          (send-new-player-info-msgs to-player-id originator-player-id (get-last-melody-event-num-for-player to-player))
          (println "players.clj - replace-complement-info - NOT SENDING MESSAGES"))
        (assoc @PLAYERS to-player-id to-player)
        )
      (do
        (println "players - replace-complement-info NOT COPYING!")
        cur-players)))
  )

(defn player-new-complement-info-replace
  [& {:keys [change-player-id follow-player originator-player-id]}]
  (send PLAYERS replace-complement-info change-player-id follow-player originator-player-id)
  )

(defn new-change-follow-info-note-for-player
  [& {:keys [change-player-id follow-player-id originator-player-id melody-no]}]
  (send PLAYERS set-change-follow-info-note change-player-id follow-player-id originator-player-id melody-no)
  )

(defn update-player-and-follow-info
  [player]
  (send PLAYERS copy-follow-info player)
  (await PLAYERS)
  )

(defn init-players
  []
;;  (register-listener MSG-PLAYER-NEW-SEGMENT player-new-segment nil)
  )

(defn print-player-melody
  [melody]
  (let [sorted-keys (sort (keys melody))]
    (println (format "%-20s" "  :melody "))
    (doseq [melody-key sorted-keys]
      (println (format "%-29s" (str "  " melody-key "-" (dissoc (get melody melody-key) :instrument-info))))
      (println (format
                "%-29s"
                (str "  " melody-key
                     ":instrument-name:" (:name (:instrument (:instrument-info (get melody melody-key))))
                     " range-lo: " (:range-lo (:instrument-info (get melody melody-key)))
                     " range-hi: " (:range-hi (:instrument-info (get melody melody-key))) )))
      )))

(defn print-player
  "Pretty Print a player map

  player - the player map to print"
  [player & {:keys [prnt-full-inst-info]
             :or {prnt-full-inst-info false}}]
  (let [sorted-keys (sort (keys player))]
    (println "player:")
    (doseq [player-key sorted-keys]
      (cond
       (and (= player-key :instrument-info) (= prnt-full-inst-info false))
       (do
         (println (format "%-29s" (str "  " player-key " :name")) "-" (:name (:instrument (:instrument-info player))))
         (println (format "%-29s" (str "  " player-key " :range-lo")) "-" (:range-lo (:instrument-info player)))
         (println (format "%-29s" (str "  " player-key " :range-hi")) "-" (:range-hi (:instrument-info player))))

        (= player-key :melody)
        (print-player-melody (:melody player))

       :else
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
