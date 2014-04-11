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

(ns transport.melody
  (:use
   [transport.behaviors :only [get-behavior-action-for-player get-behavior-ensemble-action-for-player get-behavior-player-id-for-player]]
   [transport.pitch :only [get-scale-degree next-pitch]]
   [transport.ensemble-status :only [ get-average-volume get-rest-probability]]
   [transport.players]
   [transport.random :only [random-int weighted-choice]]
   [transport.rhythm :only [get-dur-info-for-beats next-note-dur]]
   [transport.settings]
   [transport.volume :only [select-volume select-volume-in-range]]
   ))

(def CONTINUITY-PROBS [4 3 2 1 1 1 1 2 2 1])

(defn- select-melody-continuity
  "Returns a number from 1 to 10 to determine how continuous
   the melody will be.
   0 - continuous (few rests) -> 9 - discontinuous (all rests)"
  ([] (weighted-choice CONTINUITY-PROBS))
  ([player]
     (weighted-choice CONTINUITY-PROBS)
     )
  )

(defn- select-melody-density
  "Returns a number from 1 to 9 to determine how dense
   the melody will be.
   0 - sparse  (few notes of long duration) -> 9 - dense (many notes of short duration"
  ([] (rand-int 10))
  ([player]
     (rand-int 10)
     )
  )

(defn- select-melody-range
  "Returns a number from 1 to 9 to determine the width of
   the melody's range.
   0 - narrow range -> 9 - wide range"
  ([] (rand-int 10))
  ([player]
     (rand-int 10)
     )
  )

(defn- select-melody-smoothness
  "Returns a number from 1 to 9 to determine how smooth (stepwise)
   the melody will be.
   0 - mostly steps -> 9 - mostly skips (wide skips)"
  ([] (rand-int 10))
  ([player]
     (rand-int 10)
     )
  )

(defn select-random-melody-characteristics
  []
  {
   :continuity (select-melody-continuity)
   :density (select-melody-density)
   :range (select-melody-range)
   :smoothness (select-melody-smoothness)
   }
  )

(defn select-melody-characteristics
  [player]
  {
   :continuity (select-melody-continuity player)
   :density (select-melody-density player)
   :range (select-melody-range player)
   :smoothness (select-melody-smoothness player)
   }
  )

(defn note-or-rest
  "Determines whether to play a note or rest  next.
   Returne true for note, false for rest

   player - the player to determine note or rest for"
  [player]
  (let [play-note? (random-int 0 10)]
    (if (< (get-melody-continuity-char player) play-note?)
      true
      (if (not= 0 play-note?)                                ;; if continuity not 0
        false                                                ;; rest
        (if (and                                             ;; else
             (not= {} (get-melody player))                   ;; if melody not empty
             (= 0                                            ;; and last pitch is root
                (get-scale-degree
                 player
                 (or (get-last-melody-note player) 0)))      ;; or rest
             (< (rand) 0.8))                                 ;; possibly rest
          false
          true)))))

(defn note-or-rest-follow-ensemble
  [player]
  (if (< 0.5 (get-rest-probability)) nil true))

(defn note-or-rest-contrast-ensemble
  [player]
  (if (< 0.5 (get-rest-probability)) true nil))

(defn get-volume
  [melody-event]
  (:volume melody-event))

(defn get-melody-event
  [player-id melody-event-no]
  (get (get-melody (get-player player-id)) melody-event-no))

(defn next-melody-follow
  [player]
  (let [follow-player-id (get-behavior-player-id-for-player player)
        follow-player-last-note (get-last-melody-event-num follow-player-id)
        next-new-event {:note nil
                        :dur-info (get-dur-info-for-beats (get-player follow-player-id) 3)
                        :follow-note (if (nil? follow-player-last-note)
                                       0
                                       (- follow-player-last-note 1))
                        :volume (select-volume player)
                        }
        ]
    (if (nil? (:follow-note (get-last-melody-event player)))
      ;; first time, rest 3 beats
      next-new-event
      ;; else
      ;; play FOLLOWer melody event after last-melody event
      (let [
            last-melody-event-played (get-last-melody-event player)
            cur-note-to-play (+ (:follow-note last-melody-event-played) 1)
            next-melody-event (get-melody-event follow-player-id cur-note-to-play)
            ]
        (if (nil? next-melody-event)
          ;; unless
          ;; FOLLOWer ahead of FOLLOWed
          ;; then repeat whatever melody-event just played
          (assoc last-melody-event-played :follow-note (:follow-note last-melody-event-played))
          (assoc next-melody-event :follow-note cur-note-to-play))
        )))
  )

(defn- next-melody-complement-ensemble
  [player]
  (let [next-note-or-rest (if (note-or-rest-follow-ensemble player) (next-pitch player) nil)
        average-volume (get-average-volume)
        ]
    (println "average-volume: " average-volume)
    {:note next-note-or-rest
     :dur-info (next-note-dur player)
     :volume (select-volume-in-range
              (if (<= average-volume 0.1) 0 (- average-volume 0.1)) ;; set range of volume to
              (if (> average-volume 0.9) 1 (+ average-volume 0.1))) ;; + or - 0.1 of average volume
     }
    ))

(defn- next-melody-contrast-ensemble
  [player]
  (let [next-note-or-rest (if (note-or-rest-contrast-ensemble player) (next-pitch player) nil)
        average-volume (get-average-volume)
        ]
    (println "average-volume: " average-volume)
    {:note next-note-or-rest
     :dur-info (next-note-dur player)
     :volume (select-volume-in-range
              (if (< average-volume 0.5) 0.5 0)  ;; set range of volume eithe 0.5 - 1 or
              (if (< average-volume 0.5) 1 0.5)) ;; 0 - 0.5 opposite of ensemble
     }
    ))

(defn- next-melody-for-player
  [player]
  (let [play-note? (note-or-rest player)
        ]
       {:note (if play-note? (next-pitch player) nil)
        :dur-info (next-note-dur player)
        :volume (if play-note? (select-volume player) 0) ;; 0 volume if rest
        })
  )

(defn next-melody
  "Returns the next note information as a map for player

    player - the player map"
  [player]
  (cond
   (= (get-behavior-action-for-player player) FOLLOW) (next-melody-follow player)
   (= (get-behavior-ensemble-action-for-player player) COMPLEMENT) (next-melody-complement-ensemble player)
   (= (get-behavior-ensemble-action-for-player player) CONTRAST) (next-melody-contrast-ensemble player)
   :else (next-melody-for-player player))  ;; pick next melody note based only on players settings
                                           ;;  do not reference other players or ensemble
  )
