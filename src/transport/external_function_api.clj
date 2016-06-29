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
   [transport.curplayer :refer [get-updated-player set-updated-player]]
   [transport.play-note :refer [cur-player-info update-based-on-ensemble
                                update-player-with-new-segment]]
   [transport.players :refer [get-behavior get-player-map
                              update-player-follow-info]]
   )
  )

(defn update-ensemble-info-for-player
  []
  (swap! cur-player-info
         set-updated-player
         (update-based-on-ensemble (get-updated-player @cur-player-info)))
  )

(defn update-follow-info-for-player
  []
  (let [player (get-updated-player @cur-player-info)
        updated-player
        (update-player-follow-info
         player
         (get-player-map (get-behavior-player-id (get-behavior player)))
         )
        ]

    (swap! cur-player-info set-updated-player updated-player)
    )
  )
(defn update-segment-for-player
  []
  (swap! cur-player-info
         set-updated-player
         (update-player-with-new-segment (get-updated-player @cur-player-info)
                                         (:event-time @cur-player-info)
                                         )
         )
  )
