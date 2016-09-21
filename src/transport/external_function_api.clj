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
   [transport.behavior :refer [get-behavior-player-id]]
   [transport.curplayer :refer [get-current-player set-updated-player]]
   [transport.play-note :refer [cur-player-info update-based-on-ensemble
                                update-player-with-new-segment]]
   [transport.players :refer [get-behavior get-player-map
                              update-player-follow-info]]
   )
  )

(defn- update-cur-player-ensemble-info
  [cur-player]
  (set-updated-player cur-player
                      (update-based-on-ensemble (get-current-player cur-player)))
  )

(defn update-ensemble-info-for-player
  []
  (swap! cur-player-info update-cur-player-ensemble-info)
  )

(defn- update-cur-player-follow-info
  [cur-player]
  (let [player (get-current-player cur-player)
        updated-player
        (update-player-follow-info
         player
         (get-player-map (get-behavior-player-id (get-behavior player)))
         )
        ]
    (set-updated-player cur-player updated-player)
    )
  )

(defn update-follow-info-for-player
  []
  (swap! cur-player-info update-cur-player-follow-info)
  )

(defn- update-cur-player-segment
  [cur-player]
  (set-updated-player cur-player
                      (update-player-with-new-segment (get-current-player cur-player)
                                                      (:event-time cur-player)
                                                      ))
  )

(defn update-segment-for-player
  []
  (swap! cur-player-info update-cur-player-segment)
  )
