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

(ns transport.ensemble
  (:require
   [transport.behavior :refer :all]
   [transport.players :refer :all]
   [transport.settings :refer :all]
   [transport.util.utils :refer :all]
   )
  )

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

(defn reset-ensemble
  []
  (swap! ensemble clear-ensemble)
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

(defn init-ensemble
  []
  (let [all-players (map create-player (range @number-of-players))
        ;; set the :behavior :player-id for all players that are FOLLOWing, SIMILARing or CONTRASTing other players
        final-players (zipmap
                       (map get all-players (repeat :player-id))
                       (map atom
                            (map assoc
                                 all-players
                                 (repeat :behavior)
                                 (map select-and-set-behavior-player-id
                                      all-players
                                      (repeat :all-players)
                                      (repeat all-players))
                                 )))
        ]
    (reset-players)
    (swap! ensemble conj final-players)
    )

  (print-all-players)

  ;; Schedule first event for all players
  (dorun (map sched-event
              (repeat 0)
              (map get-player-val (get-ensemble) (repeat "function"))
              (map get-player-id (get-ensemble))))
  )
