;    Copyright (C) 2013-2015  Joseph Fosco. All Rights Reserved
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
   [transport.behavior :refer [get-behavior-action get-behavior-player-id]]
   [transport.instrumentinfo :refer [get-all-instrument-info]]
   [transport.melodyevent :refer [get-follow-note-for-event get-instrument-info-for-event get-sc-instrument-id set-note-play-time-for-event]]
   [transport.message-processor :refer [send-message register-listener]]
   [transport.messages :refer :all]
   [transport.settings :refer :all]
   [transport.util.utils :refer :all]
   )
  (:import transport.behavior.Behavior)
  )

(def PLAYERS (atom {}))

(defn get-players
  []
  (vals @PLAYERS))

(defn get-player
  [player-id]
  (get @PLAYERS player-id))

(defn get-player-val
  "Returns the requested value for the specified player

   player - the player to return the value from
   val - string name of the value to return"
  [player val]
  (get player (keyword val))
  )

(defn get-behavior
  [player]
  (:behavior player))

(defn get-change-follow-info-notes
  [player]
  (:change-follow-info-notes player))

(defn get-next-change-follow-info-note
  [player]
  (first (:change-follow-info-notes player)))

(defn get-function
  [player]
  (:function player))

(defn get-cur-note-beat
  [player]
  (:cur-note-beat player))

(defn get-cur-note-time
  [player]
  (:cur-note-time player))

(defn get-prev-note-time
  [player]
  (:prev-note-time player))

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

(defn get-seg-len
  [player]
  (:seg-len player))

(defn get-seg-num
  [player]
  (:seg-num player))

(defn get-seg-start
  [player]
  (:seg-start player))

(defn get-sync-beat-player-id
  [player]
  (:sync-beat-player-id player))

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
      (:note (get cur-melody (get-last-melody-event-num-for-player player)))))
  )

(defn get-last-melody-event
  [player]
  (let [player-melody (get-melody player)]
    (if (= player-melody {}) nil (get player-melody (get-last-melody-event-num-for-player player)))
    )
  )

(defn get-melody-event-num
  "Returns the players melody event for melody-event-num (a number).
   Returns nil if the key does not exist for the player."
  [player melody-event-num]
  (get (get-melody player) melody-event-num)
  )

(defn get-player-with-mm
  "Returns player-id for the player with requested mm.
   Does not return requesting-player id.
   Returns nil if no other players match mm"
  [requesting-player mm]
  (let [requesting-player-id (get-player-id requesting-player)]
    (loop [player-id-to-check 0]
      (cond (= player-id-to-check @number-of-players)
            nil
            (and (= mm (get-mm (get-player player-id-to-check))) (not= player-id-to-check requesting-player-id))
            player-id-to-check
            :else (recur (inc player-id-to-check))
        )
      )
    )
  )

(defn clear-players
  "used by send or send-off to clear agents"
  [cur-players]
  {}
  )

(defn reset-players
  []
  (swap! PLAYERS clear-players)
  )

(defn set-behavior
  [player behavior]
  (assoc player :behavior behavior)
  )

(defn set-behavior-player-id
  "Returns new :behavior map with :player-id set to player-id

   player - tnhe player whose :behavior is to be changed
   player-id - the player-id to set :player-id to"
  [player player-id]
  (assoc (:behavior player)
    :player-id player-id)
  )

(defn set-function
  [player function]
  (assoc player :function function)
  )

(defn update-melody-event-note-time-callback
  [cur-players player-id melody-event-num note-play-time]

  (let [player (get-player player-id)
        melody (get-melody player)
        ]
       (assoc cur-players
              player-id
              (assoc player
                :melody
                (assoc
                  melody
                  melody-event-num
                  (set-note-play-time-for-event (get-melody-event-num player melody-event-num) note-play-time)
                  )
                )
         ))
  )

(defn update-player-callback
  "update the value of a player in agent PLAYERS
   this is called from send-off"
  [cur-players new-player]
  (assoc cur-players (get-player-id new-player) new-player)
  )

(defn update-player
  [player]
  (swap! PLAYERS update-player-callback player)
  player
  )

(defn rand-player-id-excluding-player
  "Select a random player-id not including the
   player-id of player

   player - player to exclude from possible player-ids"
  [player & {:keys [player-map]
      :or {player-map @PLAYERS}}]
  (if (> (count player-map) 0)
    (rand-nth (keys (dissoc player-map (:player-id player))))
    nil
    ))

(defn get-similar-info-from-player
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
  (assoc (get-similar-info-from-player follow-player)
    :instrument-info (get-instrument-info follow-player)
    )
  )

(defn set-change-follow-info-note
  [cur-players from-player-id to-player-id originator-player-id melody-no]
  (if (not= originator-player-id to-player-id)
    (let [to-player (get-player to-player-id)]
      (if (and (= from-player-id (get-player-id (:behavior to-player)))
               (not (some #{melody-no} (get-change-follow-info-notes to-player)))
               )
        (assoc cur-players to-player-id
               (assoc to-player :change-follow-info-notes (conj (get-change-follow-info-notes to-player) melody-no)))
        cur-players))
    cur-players
    )
  )

(defn- send-msg-new-player-info
  [change-player-id originator-player-id melody-no]
  (send-message MSG-PLAYER-NEW-FOLLOW-INFO
                :change-player-id change-player-id
                :originator-player-id  originator-player-id
                :melody-no melody-no)
  (send-message MSG-PLAYER-NEW-SIMILAR-INFO
                :change-player-id change-player-id
                :originator-player-id  originator-player-id)
  (send-message MSG-PLAYER-NEW-CONTRAST-INFO
                :change-player-id change-player-id
                :originator-player-id  originator-player-id)
  )

(defn set-new-contrast-info
  [cur-players change-player-id contrasting-player-id originator-player-id new-contrasting-info-map]
  (let [contrasting-player (get-player contrasting-player-id)]
    (if (= change-player-id (get-player-id (:behavior contrasting-player)))
      (do
        (if (not= originator-player-id contrasting-player-id)
          (do
            (send-msg-new-player-info contrasting-player-id originator-player-id (get-last-melody-event-num-for-player contrasting-player))
            (assoc cur-players contrasting-player-id (merge contrasting-player new-contrasting-info-map))
            )
          )
        cur-players)
      (do
        cur-players)))
  )

(defn new-contrast-info-for-player
  [& {:keys [change-player-id contrast-player-id originator-player-id contrasting-info]}]
  (print-msg "new-contrast-info-for-player" "contrast-player-id: " contrast-player-id)
  (swap! PLAYERS
        set-new-contrast-info
        change-player-id
        contrast-player-id
        originator-player-id
        contrasting-info
        )
  )

(defn replace-similar-info
  [cur-players from-player-id to-player originator-player-id]
  (let [to-player-id (get-player-id to-player)]
    (if (= from-player-id (get-player-id (:behavior to-player)))
      (do
        (if (not= originator-player-id to-player-id)
          (send-msg-new-player-info to-player-id originator-player-id (get-last-melody-event-num-for-player to-player))
          )
        (assoc cur-players to-player-id to-player)
        )
      (do
        cur-players)))
  )

(defn player-new-similar-info-replace
  [& {:keys [change-player-id follow-player originator-player-id]}]
  (swap! PLAYERS replace-similar-info change-player-id follow-player originator-player-id)
  )

(defn new-change-follow-info-note-for-player
  [& {:keys [change-player-id follow-player-id originator-player-id melody-no]}]
  (print-msg "new-change-follow-info-note-for-player" "change-player-id: " change-player-id " follow-player-id: " follow-player-id)
  (swap! PLAYERS set-change-follow-info-note change-player-id follow-player-id originator-player-id melody-no)
  )

(declare print-player)
(defn update-player-follow-info
  [to-player melody-event-num]
  (let [to-player-id (get-player-id to-player)
        from-player-id (get-behavior-player-id (get-behavior to-player))
        cur-change-follow-info-note (get-next-change-follow-info-note to-player)
        last-follow-note (get-follow-note-for-event (get-last-melody-event to-player))
        ]
    (if (and
         (not (nil? from-player-id))
         (not (nil? last-follow-note))
         (not (nil? cur-change-follow-info-note))
         (= (inc last-follow-note) cur-change-follow-info-note))
      (let [updated-player (merge to-player
                                  (get-following-info-from-player (get-player from-player-id))
                                  )]

        (send-msg-new-player-info to-player-id to-player-id melody-event-num)
        updated-player)
      (do
        (binding [*out* *err*]
                 (print-msg "update-player-and-follow-info" "COPY FOLLOW-INFO ERROR   COPY FOLLOW-INFO ERROR   COPY FOLLOW-INFO ERROR   ")
                 (print-player to-player)
                 (print-msg "update-player-and-follow-info" "from-player-id:   " from-player-id)
                 (print-msg "update-player-and-follow-info" "to-player-id:     " to-player-id)
                 (print-msg "update-player-and-follow-info" "last-follow-note: " last-follow-note)
                 (print-msg "update-player-and-follow-info" "cur-change-follow-info-note: " cur-change-follow-info-note)
                 )
        (throw (Throwable. "COPY FOLLOW-INFO ERROR"))
        )
      ))
  )

(defn print-player-melody
  [melody & {:keys [prnt-full-inst-info]
             :or {prnt-full-inst-info false}}]
  (let [sorted-keys (sort (keys melody))]
    (println (format "%-20s" "  :melody "))
    (doseq [melody-key sorted-keys]
      (println (format "%-29s" (str "  " melody-key "-" (dissoc (get melody melody-key) :instrument-info))))
      (if prnt-full-inst-info
        (do
          (println (format
                    "%-29s"
                    (str "  " melody-key "-"
                         ":instrument-info:" (get-all-instrument-info (get (get melody melody-key) :instrument-info))
                         )
                    ))
          )
        (println (format
                  "%-29s"
                  (str "  " melody-key "-"
                       ":instrument-name:" (:name (:instrument (:instrument-info (get melody melody-key))))
                       " range-lo: " (:range-lo (:instrument-info (get melody melody-key)))
                       " range-hi: " (:range-hi (:instrument-info (get melody melody-key))) ))))
      )
    )
  )

(defn print-player
  "Pretty Print a player map

  player - the player map to print"
  [player & {:keys [prnt-full-inst-info]
             :or {prnt-full-inst-info false}}]
  (let [sorted-keys (sort (keys player))]
    (println "player: " (get-player-id player) "current time: " (System/currentTimeMillis))
    (doseq [player-key sorted-keys]
      (cond
       (and (= player-key :instrument-info) (= prnt-full-inst-info false))
       (do
         (println (format "%-29s" (str "  " player-key " :name")) "-" (:name (:instrument (:instrument-info player))))
         (println (format "%-29s" (str "  " player-key " :range-lo")) "-" (:range-lo (:instrument-info player)))
         (println (format "%-29s" (str "  " player-key " :range-hi")) "-" (:range-hi (:instrument-info player))))

        (= player-key :melody)
        (print-player-melody (:melody player) :prnt-full-inst-info prnt-full-inst-info)

       :else
        (println (format "%-20s" (str "  " player-key)) "-" (get player player-key)))
      )
    (println "end player: " (get-player-id player) "current time: " (System/currentTimeMillis))
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
(defn print-all-actions
  []
  (dorun
   (map #(let [plyr-action (get-behavior-action (get-behavior %1))]
           (cond (= plyr-action IGNORE) (println "IGNORE")
                 (= plyr-action CONTRAST-PLAYER) (println "CONTRAST-PLAYER")
                 (= plyr-action SIMILAR-PLAYER) (println "SIMILAR-PLAYER")
                 (= plyr-action FOLLOW-PLAYER) (println "FOLLOW-PLAYER")
                 (= plyr-action SIMILAR-ENSEMBLE) (println "SIMILAR-ENSEMBLE")
                 (= plyr-action CONTRAST-ENSEMBLE) (println "CONTRAST-ENSEMBLE")
                 )
           )
        (vals @PLAYERS)
        )))
