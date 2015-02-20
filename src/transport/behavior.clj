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

(ns transport.behavior)

(defrecord Behavior [accuracy action player-id])

(defn get-behavior-action
  [behavior]
  (:action behavior))

(defn get-behavior-player-id
  [behavior]
  (:player-id behavior))

(defn set-behavior-player-id
  "Returns new Behavior record with :player-id set to player-id

   behavior - the current Behavior record whose :player-id is to be changed
   player-id - the player-id to set :player-id to"
  [behavior player-id]
  (assoc behavior :player-id player-id)
  )
