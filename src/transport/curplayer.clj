;    Copyright (C) 2014-2016  Joseph Fosco. All Rights Reserved
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

(ns transport.curplayer
  (:require
   [transport.melody.melodyevent :refer [get-follow-note-for-event]]
   [transport.players :refer [get-last-melody-event
                              get-next-change-follow-info-note get-player
                              get-player-id get-seg-len get-seg-start]]
   )
  )

(defrecord CurPlayer [player-id player event-time updated-player new-segment?
                      new-follow-info?])

(defn- check-new-follow-info
  "Returns true if the event after player's last melody-event
   is a new segment in the following player.
   Compares the seg-num in the molody-event of the FOLLOWing player
   for the melody event passed in (or FOLLOWer's last melody-event)
   with the seg-num of the FOLLOWing player's next melody event.
   If they do not match, it is a new-seg in the FOLLOWing player.

   player - map for the player to check
   melody event - melody event to check or player's last melody event if omitted"
  [player & {:keys [increment]
             :or {increment 0}}]
  (if
      (and (get-next-change-follow-info-note player)
           (>= (+ (get-follow-note-for-event (get-last-melody-event player))
                  increment)
               (get-next-change-follow-info-note player)))
    true
    false)
  )

(defn create-curplayer
  [player-id event-time]
  (let [player (deref (get-player player-id))
        new-segment? (if (>= event-time
                             (+  (get-seg-start player)
                                (get-seg-len player)))
                   true
                   false
                   )
        new-follow-info? (if (not new-segment?)
                             (check-new-follow-info player :increment 1)
                             false
                             )
        ]
    (CurPlayer. player-id player event-time nil new-segment? new-follow-info?))
  )

(defn get-original-player
  "Returns the original player this record was created with

   curplayer - the curplayer record to get updated-player from"
  [curplayer]
  (:player curplayer)
  )

(defn set-updated-player
  "Returns new CurPlayer record with :updated-player set to player

   curplayer - the CurPlayer record whose :updated-player is to be set
   player - the player to set :updated-player to"
  [curplayer player]
  (assoc curplayer :updated-player player)
  )

(defn get-current-player
  "If a updated-player has not yet been set, returns player,
   else returns updated-player

   curplayer - the curplayer record to get updated-player from"
  [curplayer]
  (or (:updated-player curplayer) (:player curplayer))
  )

(defn get-cur-player-id
  [curplayer]
  (get-player-id (get-current-player curplayer))
  )

(defn get-event-time
  [curplayer]
  (:event-time curplayer)
  )

(defn new-segment?
  [curplayer]
  (:new-segment? curplayer)
  )

(defn new-follow-info?
  [curplayer]
  (:new-follow-info? curplayer)
  )
