;    Copyright (C) 2015  Joseph Fosco. All Rights Reserved
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

(ns transport.melody.liveplayermelodyevent )

(defrecord LivePlayerMelodyEvent [note dur-millis player-id sc-instrument-id start-time volume])

(defn create-live-player-melody-event
  [& {:keys [note dur-millis player-id sc-instrument-id start-time volume]}]
  (LivePlayerMelodyEvent. note
                          dur-millis
                          player-id
                          sc-instrument-id
                          start-time
                          volume)
  )

(defn get-live-player-note
  [live-player-melody-event]
  (:note live-player-melody-event)
  )

(defn get-live-player-sc-instrument-id
  [live-player-melody-event]
  (:sc-instrument-id live-player-melody-event)
  )

(defn get-live-player-start-time
  [live-player-melody-event]
  (:start-time live-player-melody-event)
  )
