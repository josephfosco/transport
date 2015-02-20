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

(ns transport.player-copy
  (:require
   [transport.behavior :refer [get-behavior-player-id]]
   [transport.ensemble :refer :all]
   [transport.instrument :refer [get-instrument-range-hi get-instrument-range-lo]]
   [transport.melodychar :refer [get-melody-char-range-lo get-melody-char-range-hi]]
   [transport.players :refer :all]
   [transport.util.utils :refer :all]
   ))

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
        (player-new-similar-info-replace
         :change-player-id change-player-id
         :follow-player-id follow-player-id
         :originator-player-id originator-player-id
         :similar-info new-similar-info
         )
        )
      ))
  )
