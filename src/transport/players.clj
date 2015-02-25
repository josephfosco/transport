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
   [transport.behavior :refer [get-behavior-action get-behavior-player-id set-behavior-player-id]]
   [transport.instrument :refer [get-instrument-range-hi get-instrument-range-lo]]
   [transport.instrumentinfo :refer [get-all-instrument-info]]
   [transport.melodychar :refer [get-melody-char-range-hi get-melody-char-range-lo]]
   [transport.melodyevent :refer [get-follow-note-for-event get-instrument-info-for-event get-sc-instrument-id]]
   [transport.message-processor :refer [send-message register-listener]]
   [transport.messages :refer :all]
   [transport.settings :refer :all]
   [transport.util.utils :refer :all]
   )
  (:import transport.behavior.Behavior)
  )

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

(defn set-behavior
  [player behavior]
  (assoc player :behavior behavior)
  )

(defn set-function
  [player function]
  (assoc player :function function)
  )

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
  [cur-contrasting-player change-player-id originator-player-id new-contrasting-info-map]
  (if (= change-player-id (get-player-id (:behavior cur-contrasting-player)))
    (do
      (if (not= originator-player-id (get-player-id cur-contrasting-player))
        (do
          (send-msg-new-player-info (get-player-id cur-contrasting-player)
                                    originator-player-id
                                    (get-last-melody-event-num-for-player cur-contrasting-player))
          (merge cur-contrasting-player new-contrasting-info-map)
          )
        )
      cur-contrasting-player)
    cur-contrasting-player)
  )

(defn replace-similar-info
  [cur-to-player from-player-id originator-player-id new-similar-info]
  (let [to-player-id (get-player-id cur-to-player)]
    (if (= from-player-id (get-player-id (get-behavior cur-to-player)))
      (do
        (if (not= originator-player-id to-player-id)
          (send-msg-new-player-info to-player-id
                                    originator-player-id
                                    (get-last-melody-event-num-for-player cur-to-player))
          )
        (merge cur-to-player new-similar-info)
        )
      cur-to-player))
  )

(defn set-change-follow-info-note
  [cur-to-player from-player-id originator-player-id melody-no]
  (if (not= originator-player-id (get-player-id cur-to-player))
    (if (and (= from-player-id (get-player-id (:behavior cur-to-player)))
             (not (some #{melody-no} (get-change-follow-info-notes cur-to-player)))
             )
      (assoc cur-to-player :change-follow-info-notes (conj (get-change-follow-info-notes cur-to-player) melody-no))
      cur-to-player)
    cur-to-player
    )
  )

(declare print-player)
(defn update-player-follow-info
  [to-player from-player melody-event-num]
  (let [to-player-id (get-player-id to-player)
        from-player-id (get-behavior-player-id (get-behavior to-player))
        cur-change-follow-info-note (get-next-change-follow-info-note to-player)
        last-follow-note (get-follow-note-for-event (get-last-melody-event to-player))
        ]
    (if (and
         (not (nil? from-player-id))
         (not (nil? last-follow-note))
         (not (nil? cur-change-follow-info-note))
         (= from-player-id (get-player-id from-player))
         (= (inc last-follow-note) cur-change-follow-info-note))
      (let [updated-player (merge to-player
                                  (get-following-info-from-player from-player)
                                  )]

        (send-msg-new-player-info to-player-id to-player-id melody-event-num)
        updated-player)
      (do
        (binding [*out* *err*]
                 (print-msg "update-player-and-follow-info" "COPY FOLLOW-INFO ERROR   COPY FOLLOW-INFO ERROR   COPY FOLLOW-INFO ERROR   ")
                 (print-player to-player)
                 (print-player from-player)
                 (print-msg "update-player-follow-info" "from-player-id:   " (get-player-id from-player))
                 (print-msg "update-player-follow-info" "to-player-id:     " to-player-id)
                 (print-msg "update-player-follow-info" "last-follow-note: " last-follow-note)
                 (print-msg "update-player-follow-info" "cur-change-follow-info-note: " cur-change-follow-info-note)
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

(defn print-player-long
  "Pretty Print a player map with all instrument-info

  player - the player map to print"
  [player]
  (print-player player :prnt-full-inst-info true)
  )

(defn print-player-action
  [player]
  (let [plyr-action (get-behavior-action (get-behavior player))]
    (cond (= plyr-action IGNORE) (println "IGNORE")
          (= plyr-action CONTRAST-PLAYER) (println "CONTRAST-PLAYER")
          (= plyr-action SIMILAR-PLAYER) (println "SIMILAR-PLAYER")
          (= plyr-action FOLLOW-PLAYER) (println "FOLLOW-PLAYER")
          (= plyr-action SIMILAR-ENSEMBLE) (println "SIMILAR-ENSEMBLE")
          (= plyr-action CONTRAST-ENSEMBLE) (println "CONTRAST-ENSEMBLE")
          )
    )
  )


;; --------------------------------------------------------------------------------------

(def ensemble (atom {}))

(defn get-ensemble
  []
  (map deref (vals @ensemble)))

(defn get-player
  [player-id]
  (get @ensemble player-id))

(defn get-player-map
  [player-id]
  (deref (get @ensemble player-id)))

(defn get-player-with-mm
  "Returns player-id for the player with requested mm.
   Does not return requesting-player id.
   Returns nil if no other players match mm"
  [requesting-player mm]
  (let [requesting-player-id (get-player-id requesting-player)]
    (loop [player-id-to-check 0]
      (cond (= player-id-to-check @number-of-players)
            nil
            (and (= mm (get-mm (get-player-map player-id-to-check))) (not= player-id-to-check requesting-player-id))
            player-id-to-check
            :else (recur (inc player-id-to-check))
        )
      )
    )
  )

(defn rand-player-id-excluding-player
  "Select a random player-id not including the
   player-id of player

   player - player to exclude from possible player-ids"
  [player & {:keys [all-players]
             :or {all-players (map deref (vals @ensemble))}}]
  (if (> (count all-players) 1)
    (get-player-id (rand-nth (remove #(= % player) all-players)))
    nil
    ))

(defn select-and-set-behavior-player-id
  "If :behavior :action is not IGNORE, SIMILAR-ENSEMBLE or CONTRAST-ENSEMBLE
   returns a :behavior map with :player-id selected from map of players
   passed in as all-players.
   if :behavior is IGNORE returns the current :behavior map

   player - the player to set :behavior :player-id"
  [player & {:keys [all-players]}]
  (let [player-action (get-behavior-action (get-behavior player))]
    (if (and (not= player-action SIMILAR-ENSEMBLE)
             (not= player-action CONTRAST-ENSEMBLE)
             (not= player-action IGNORE))
      (let [player-id (rand-player-id-excluding-player player :all-players all-players)]
        (set-behavior-player-id (get-behavior player) player-id)
        )
      (get-behavior player)
      )))

(defn update-player-callback
  "update the value of a player in atom ensemble
   this is called from send-off"
  [cur-players new-player]
  (assoc cur-players (get-player-id new-player) (atom new-player))
  )

(defn update-player
  [player]
  (swap! ensemble update-player-callback player)
  player
  )

(defn new-change-follow-info-note-for-player
  [& {:keys [change-player-id follow-player-id originator-player-id melody-no]}]
  (print-msg "new-change-follow-info-note-for-player" "change-player-id: " change-player-id " follow-player-id: " follow-player-id " melody-no: " melody-no)
  (swap! (get-player follow-player-id) set-change-follow-info-note change-player-id originator-player-id melody-no)
  )

(defn new-contrast-info-for-player
  [& {:keys [change-player-id contrast-player-id originator-player-id contrasting-info]}]
  (print-msg "new-contrast-info-for-player" "contrast-player-id: " contrast-player-id)
  (swap! (get-player contrast-player-id)
        set-new-contrast-info
        change-player-id
        originator-player-id
        contrasting-info
        )
  )

(defn new-similar-info-for-player
  [& {:keys [change-player-id follow-player-id originator-player-id similar-info]}]
  (swap! (get-player follow-player-id)
         replace-similar-info
         change-player-id
         originator-player-id
         similar-info
         )
  )

(defn clear-ensemble
  "used by send or send-off to clear agents"
  [cur-players]
  {}
  )

(defn print-player-num
  [player-id]
  (print-player (get-player-map player-id))
  )

(defn print-all-actions
  []
  (dorun
   (map print-player-action (vals @ensemble)))
  )

(defn print-all-players
  []
  (dorun (map print-player (get-ensemble)))
)


;;---------------------------- old player_copt.clj ----------------------------------------------------------


(defn adjust-melody-char-for-instrument
  [new-melody-char instrument-info]
  (let [new-melody-lo (if (<=
                           (get-instrument-range-lo instrument-info)
                           (get-melody-char-range-lo new-melody-char)
                           (get-instrument-range-hi instrument-info)
                           )
                        (get-melody-char-range-lo new-melody-char)
                        (get-instrument-range-lo instrument-info)
                        )
        new-melody-hi (if (> (get-instrument-range-hi instrument-info)
                             (get-melody-char-range-hi new-melody-char)
                             new-melody-lo
                             )
                        (get-melody-char-range-hi new-melody-char)
                        (get-instrument-range-hi instrument-info)
                        )

        new-melody-char (assoc new-melody-char
                          :range (list new-melody-lo new-melody-hi))
        ]
    new-melody-char
    )
     )

(defn player-copy-new-similar-info
  [& {:keys [change-player-id follow-player-id originator-player-id]}]
  (let [to-player (get-player-map follow-player-id)]
    (if (= change-player-id (get-behavior-player-id (get-behavior to-player)))
      (let [similar-player-info (get-similar-info-from-player (get-player-map change-player-id))
            similar-melody-char (adjust-melody-char-for-instrument
                                    (get-melody-char similar-player-info)
                                    (get-instrument-info to-player))
           new-similar-info (assoc similar-player-info :melody-char similar-melody-char)
            ]
        (print-msg "player-copy-new-similar-info" "follow-player-id: " follow-player-id)
        (new-similar-info-for-player
         :change-player-id change-player-id
         :follow-player-id follow-player-id
         :originator-player-id originator-player-id
         :similar-info new-similar-info
         )
        )
      ))
  )
