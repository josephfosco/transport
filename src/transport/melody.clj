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
   [transport.pitch :only [next-pitch]]
   [transport.ensemble-status :only [ get-average-volume get-rest-probability]]
   [transport.players :only [get-behavior-action get-behavior-ensemble-action get-behavior-player-id get-dur-info get-last-melody-event get-melody get-melody-continuity get-note get-player get-player-id]]
   [transport.random :only [random-int]]
   [transport.rhythm :only [get-dur-info-for-beats next-note-dur]]
   [transport.settings]
   [transport.volume :only [select-volume select-volume-in-range]]
   ))

(defn select-melody-continuity
  "Returns a number from 1 to 10 to determine how continuous
   the melody will be.
   0 - continuous (few rests) -> 10 - discontinuous (all rests)"
  [player]
  (random-int 0 10)
  )

(defn select-melody-density
  "Returns a number from 1 to 9 to determine how dense
   the melody will be.
   0 - sparse  (few notes of long duration) -> 9 - dense (many notes of short duration"
  [player]
  (rand-int 9)
  )

(defn select-melody-range
  "Returns a number from 1 to 9 to determine the width of
   the melody's range.
   0 - narrow range -> 9 - wide range"
  [player]
  (rand-int 9)
  )

(defn select-melody-smoothness
  "Returns a number from 1 to 9 to determine how smooth (stepwise)
   the melody will be.
   0 - mostly steps -> 9 - mostly skips (wide skips)"
  [player]
  (rand-int 9)
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
   If player is supposed to rest, returns nil
   player - the player to determine note or rest for"
  [player]
  (let [play-note? (random-int 0 10)]
    (if (> (get-melody-continuity player) play-note?)
      true
      nil)))

(defn note-or-rest-follow-ensemble
  [player]
  (if (< 0.5 (get-rest-probability)) nil true))

(defn note-or-rest-contrast-ensemble
  [player]
  (if (< 0.5 (get-rest-probability)) true nil))

(defn get-last-melody-event-num
  [player-id]
  (let [last-melody-key (reduce max 0 (keys (get-melody (get-player player-id))))]
    (if (= last-melody-key 0) nil last-melody-key)
    )
  )

(defn get-volume
  [melody-event]
  (:volume melody-event))

(defn get-melody-event
  [player-id melody-event-no]
  (get (get-melody (get-player player-id)) melody-event-no))

(defn next-melody-follow
  [player]
  (let [follow-player-id (get-behavior-player-id player)
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

(defn next-melody-follow-ensemble
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

(defn next-melody-contrast-ensemble
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

(defn next-melody-ignore
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
   (= (get-behavior-action player) FOLLOW) (next-melody-follow player)
   (= (get-behavior-ensemble-action player) COMPLEMENT) (next-melody-follow-ensemble player)
   (= (get-behavior-ensemble-action player) CONTRAST) (next-melody-contrast-ensemble player)
   :else (next-melody-ignore player))
  )
