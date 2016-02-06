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

(defrecord CurPlayer [player-id player event-time new-player])

(defn create-curplayer
  [player-id player event-time]
  (CurPlayer. player-id player event-time nil)
  )

(defn set-new-player
  "Returns new CurPlayer record with :new-player set to player

   curplayer - the CurPlayer record whose :new-player is to be set
   player - the player to set :new-player to"
  [curplayer player]
  (assoc curplayer :new-player player)
  )
