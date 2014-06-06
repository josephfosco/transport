;    Copyright (C) 2013-2014  Joseph Fosco. All Rights Reserved
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

(ns transport.player
  (:require
   [transport.instrument :refer [get-instrument-range-hi get-instrument-range-lo]]
   [transport.melodychar :refer [get-melody-char-range-lo get-melody-char-range-hi]]
   [transport.players :refer [get-instrument get-player]]
   ))

(defn adjust-melody-char-for-instrument
  [new-melody-char instrument]
     (let [new-melody-lo (if (<=
                              (get-instrument-range-lo instrument)
                              (get-melody-char-range-lo new-melody-char)
                              (get-instrument-range-hi instrument)
                              )
                           (get-melody-char-range-lo new-melody-char)
                           (get-instrument-range-lo instrument)
                           )
           new-melody-hi (if (> (get-instrument-range-hi instrument)
                                (get-melody-char-range-hi new-melody-char)
                                new-melody-lo
                                )
                           (get-melody-char-range-hi new-melody-char)
                           (get-instrument-range-hi instrument)
                           )

           new-melody-char (assoc new-melody-char
                             :range (list new-melody-lo new-melody-hi))
           ]
       new-melody-char
       )
     )
(defn copy-follow-complement-info
  [cur-players from-player-id to-player-id originator-player-id]
  (println "players - copy-follow-complement-info from:" from-player-id "to:" to-player-id "originator:" originator-player-id)
  (let [to-player (get-player to-player-id)]
    (if (= from-player-id (:player-id (:behavior to-player)))
      (do
        (if (not= originator-player-id to-player-id)
          (do
            (send-message MSG-PLAYER-NEW-FOLLOW-INFO :change-player-id to-player-id :originator-player-id  originator-player-id)
            (send-message MSG-PLAYER-NEW-COMPLEMENT-INFO :change-player-id to-player-id :originator-player-id  originator-player-id)
            (send-message MSG-PLAYER-NEW-CONTRAST-INFO :change-player-id to-player-id :originator-player-id  originator-player-id))
          (println "players.clj - copy-follow-complement-info - NOT SENDING MESSAGES"))
        (assoc @PLAYERS to-player-id
               (merge to-player
                      (if (= (:action (:behavior to-player)) FOLLOW)
                        (get-following-info-from-player (get-player from-player-id))
                        (get-complement-info-from-player (get-player from-player-id))
                        ))))
      (do
        (println "players - copy-follow-info NOT COPYING!")
        cur-players)))
  )

(defn player-new-complement-info
  [& {:keys [change-player-id follow-player-id originator-player-id]}]
  (send PLAYERS copy-follow-complement-info change-player-id follow-player-id originator-player-id)
  (let [to-player (get-player follow-player-id)]
    (if (= from-player-id (:player-id (:behavior to-player)))
      (let [complement-player-info (get-complement-info-from-player (get-player change-player-id))
           complement-melody-char (:melody-char complement-player-info)
           new-complement-info (assoc complement-player-info
                                 :melody-char (adjust-melody-char-for-instrument
                                               complement-melody-char
                                               (get-instrument to-player)))
           ]
       (merge (assoc upd-player
                :instrument-info new-instrument
                )
              new-complement-info)
       )
      (do
        (println "players - copy-follow-info NOT COPYING!")
        )))
  )
