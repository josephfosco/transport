;    Copyright (C) 2016  Joseph Fosco. All Rights Reserved
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

(ns transport.external-function-api
  (:require
   [polyphony.core :refer :all]
   [transport.play-note :as tpn]
   )
  )

(defn play-melody-sync-player
  []
  (tpn/sync-player-play-melody)
  )

(defn play-melody-sync-ensemble
  []
  (tpn/sync-ensemble-play-melody)
  )

(defn play-melody-follow
  []
  (tpn/follow-play-melody)
  )

(defn play-melody-no-sync
  []
  (tpn/no-sync-play-melody)
  )
