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

(ns transport.segment
  (:require
   [transport.behavior :refer [get-behavior-action get-behavior-player-id]]
   [transport.behaviors :refer [select-first-behavior select-behavior]]
   [transport.dur-info :refer [get-dur-millis]]
   [transport.instrument :refer [get-instrument-range-hi get-instrument-range-lo select-instrument select-random-instrument]]
   [transport.melody :refer [select-melody-characteristics select-random-melody-characteristics]]
   [transport.melodychar :refer [get-melody-char-range-lo get-melody-char-range-hi]]
   [transport.melodyevent :refer [get-dur-info-for-event get-note-event-time-for-event get-seg-num-for-event]]
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
  (merge player (get-following-info-from-player (get-player-map (get-behavior-player-id (get-behavior player)))))
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
      :melody-char (select-random-melody-characteristics (get-instrument-range-lo new-instrument) (get-instrument-range-hi new-instrument))
      :metronome (select-metronome-mm rnd-mm)
      :mm rnd-mm
      :seg-len (select-segment-length)
      :seg-num 1
      :seg-start 0
      :scale (select-random-scale))))

(defn get-contrasting-info-for-player
  "Returns a map of key value pairs for a player that must
   CONTRAST another player

   player - player to get info for"
  [player]
  {
   :instrument-info (select-instrument player
                                       :cntrst-plyr-inst-info (-> (get-behavior player)
                                                                  (get-behavior-player-id)
                                                                  (get-player-map)
                                                                  (get-instrument-info)
                                                                  )
                                       )
   :melody-char (select-melody-characteristics player)
   }
  )

(defn new-segment?
  "Returns true if player is starting a new segment, else returns false

   player - the player to check a new segment for"
  [player]
  (let [melody-event (get-last-melody-event player)]
    (if (>= (+ (get-note-event-time-for-event melody-event) (get-dur-millis (get-dur-info-for-event melody-event)))
            (+ (get-seg-start player) (get-seg-len player)))
      true
      false))
  )

(defn new-segment
  "Returns player map with new segment info in it

   player - player map to get (and return new segment info for"
  [player]
  (let [new-behavior (select-behavior player)
        behavior-action (get-behavior-action new-behavior)
        upd-player (assoc player
                     :behavior new-behavior
                     :seg-len (select-segment-length)
                     :seg-num (inc (get-seg-num player))
                     :seg-start 0
                     :cur-note-beat 0
                     )
        ]
    (cond
     (= behavior-action FOLLOW-PLAYER)
     (let [following-player-id (get-behavior-player-id new-behavior)]
       (merge upd-player
              (get-following-info-from-player (get-player-map following-player-id))
              {:sync-beat-player-id following-player-id}))

     (= behavior-action SIMILAR-PLAYER)
     (let [similar-player-info (get-similar-info-from-player (get-player-map (get-behavior-player-id new-behavior)))
           similar-melody-char (:melody-char similar-player-info)
           new-instrument (select-instrument upd-player)
           new-melody-lo (if (<=
                              (get-instrument-range-lo new-instrument)
                              (get-melody-char-range-lo similar-melody-char)
                              (get-instrument-range-hi new-instrument)
                              )
                           (get-melody-char-range-lo similar-melody-char)
                           (get-instrument-range-lo new-instrument)
                           )
           new-melody-hi (if (> (get-instrument-range-hi new-instrument)
                                (get-melody-char-range-hi similar-melody-char)
                                new-melody-lo
                                )
                           (get-melody-char-range-hi similar-melody-char)
                           (get-instrument-range-hi new-instrument)
                           )

           new-melody-char (assoc similar-melody-char
                             :range (list new-melody-lo new-melody-hi)
                             )
           new-similar-info (assoc similar-player-info :melody-char new-melody-char)
           ]
       (merge (assoc upd-player
                :instrument-info new-instrument
                )
              new-similar-info)
       )

     :else  ;;  IGNORE or CONTRAST-PLAYER or SIMILAR-ENSEMBLE or CONTRAST-ENSEMBLE
     (let [new-instrument (if (= behavior-action CONTRAST-PLAYER)
                            (select-instrument upd-player :cntrst-plyr (get-behavior-player-id upd-player))
                            (select-instrument upd-player))
           new-mm (select-mm upd-player)
           ]
       (assoc upd-player
         :instrument-info new-instrument
         :key (select-key upd-player)
         :melody-char (select-melody-characteristics (assoc upd-player :instrument-info new-instrument))
         :metronome (select-metronome upd-player)
         :mm new-mm
         :scale (select-scale upd-player)
         :sync-beat-player-id (if (= behavior-action SIMILAR-ENSEMBLE)
                                (get-player-with-mm player new-mm)
                                nil
                                )
         )))))
