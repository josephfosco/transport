;    Copyright (C) 2014  Joseph Fosco. All Rights Reserved
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

(ns transport.curplayer)

(defrecord CurPlayer [player-id player event-time updated-player])

(defn create-curplayer
  [player-id player event-time]
  (CurPlayer. player-id player event-time nil)
  )

(defn set-updated-player
  "Returns new CurPlayer record with :updated-player set to player

   curplayer - the CurPlayer record whose :updated-player is to be set
   player - the player to set :updated-player to"
  [curplayer player]
  (assoc curplayer :updated-player player)
  )

(defn get-updated-player
  "If a updated-player has not yet been set, returns player,
   else returns updated-player

   cur-player - the curplayer record to get updated-player from"
  [curplayer]
  (or (:updated-player curplayer) (:player curplayer))
  )
