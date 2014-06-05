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

(ns transport.segment
  (:require
   [transport.behavior :refer [get-behavior-action get-behavior-player-id]]
   [transport.behaviors :refer [get-behavior-player-id-for-player select-first-behavior select-behavior]]
   [transport.instrument :refer [get-instrument-range-hi get-instrument-range-lo select-instrument select-random-instrument]]
   [transport.melody :refer [select-melody-characteristics select-random-melody-characteristics]]
   [transport.melodychar :refer [get-melody-char-range-lo get-melody-char-range-hi]]
   [transport.pitch :refer [select-key select-random-key select-scale select-random-scale]]
   [transport.players :refer :all]
   [transport.random :refer [random-int]]
   [transport.rhythm :refer [select-metronome select-metronome-mm select-mm]]
   [transport.settings :refer :all]
   ))

(def min-segment-len 10000)  ;minimum segment length in milliseconds (10 seconds)
(def max-segment-len 30000)  ;maximum segment length in milliseconds (30 seconds)

(defn select-segment-length
  []
  (random-int min-segment-len max-segment-len))


(defn copy-following-info
  [player]
  (merge player (get-following-info-from-player (get-player (get-behavior-player-id-for-player player))))
  )

(defn first-segment
  "Used only the first time a player is created.
   After the first time, use new-segment.

   player - the player to create the segment for"
  [player]
  (let [new-behavior (select-first-behavior player)
        new-instrument (select-random-instrument)
        rnd-mm (select-mm)
        ]
    (assoc player
      :behavior new-behavior
      :instrument-info new-instrument
      :key (select-random-key)
      :last-pitch nil
      :melody-char (select-random-melody-characteristics (get-instrument-range-lo new-instrument) (get-instrument-range-hi new-instrument))
      :metronome (select-metronome-mm rnd-mm)
      :mm rnd-mm
      :seg-len (select-segment-length)
      :seg-start 0
      :scale (select-random-scale))))

(defn get-contrasting-info-for-player
  "Returns a map of key value pairs for a player that must
   CONTRAST another player

   player - player to get info for"
  [player]
  {
   :instrument-info (select-instrument player)
   :melody-char (select-melody-characteristics player)
   }
  )

(defn new-segment
  [player]
  (let [new-behavior (select-behavior player)
        behavior-action (get-behavior-action new-behavior)
        upd-player (assoc player
                     :behavior new-behavior
                     :last-pitch nil
                     :seg-hi-range nil
                     :seg-lo-range nil
                     :seg-len (select-segment-length)
                     :seg-start 0
                     )
        ]
    (cond
     (= behavior-action FOLLOW)
     (merge upd-player
            (get-following-info-from-player (get-player (get-behavior-player-id new-behavior))))

     (= behavior-action COMPLEMENT)
     (let [complement-player-info (get-complement-info-from-player (get-player (get-behavior-player-id new-behavior)))
           complement-melody-char (:melody-char complement-player-info)
           new-instrument (select-instrument upd-player)
           new-melody-char (assoc complement-melody-char
                             :range (list (if (<=
                                               (get-instrument-range-lo new-instrument)
                                               (get-melody-char-range-lo complement-melody-char)
                                               (get-instrument-range-hi new-instrument)
                                               )
                                            (get-melody-char-range-lo complement-melody-char)
                                            (get-instrument-range-lo new-instrument)
                                            )
                                          (if (>
                                               (get-melody-char-range-hi complement-melody-char)
                                               (get-instrument-range-hi new-instrument))
                                            (get-instrument-range-hi new-instrument)
                                            (get-melody-char-range-hi complement-melody-char))
                                          )
                             )
           new-complement-info (assoc complement-player-info :melody-char new-melody-char)
           ]
       (println "&&&&&&&&&&")
       (println "segment.clj - new-segment (complement) Complement-player-info:" complement-player-info)
       (println "segment.clj - new-segment (complement) Complement-melody-char:" complement-melody-char)
       (println "segment.clj - new-segment (complement) new-instrument:" new-instrument)
       (println "segment.clj - new-segment (complement) new-melody-char:" new-melody-char)
       (println "segment.clj - new-segment (complement) new-complement-info:" new-complement-info)
       (println "&&&&&&&&&&")
       (merge (assoc upd-player
                :instrument-info new-instrument
                )
              new-complement-info))

     :else  ;;  IGNORE or CONTRAST
     (let [new-instrument (select-instrument upd-player)]
       (assoc upd-player
         :instrument-info new-instrument
         :key (select-key upd-player)
         :melody-char (select-melody-characteristics (assoc upd-player :instrument-info new-instrument))
         :metronome (select-metronome upd-player)
         :mm (select-mm upd-player)
         :scale (select-scale upd-player))))))
