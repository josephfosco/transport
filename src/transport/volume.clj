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

(ns transport.volume
  (:require
   [transport.behavior :refer [get-behavior-action]]
   [transport.ensemble-status :refer [get-average-volume]]
   [transport.melodychar :refer [get-melody-char-vol-smoothness]]
   [transport.melodyevent :refer [get-volume-for-event]]
   [transport.players :refer :all]
   [transport.random :refer [random-int]]
   [transport.util.constants :refer :all]
   ))

(def MIN-VOLUME 0.2)
(def volume-smoothness [0 0.01 0.03 0.05 0.07 0.1 0.15 0.2 0.25 0.3])

(defn select-volume-in-range
  "Returns a random float between lo-vol and hi-vol(exclusive).
   precision is to .999

  lo-vol - lowest value that can be returned
  hi-vol - highest value that can be returned(exclusive)"
  [lo-vol hi-vol]
  (float (/ (int (* (+ (rand (- hi-vol lo-vol)) lo-vol) 1000)) 1000))
  )

(defn- select-random-volume
  "Returns a random volume between .2 and .999"
  []
  ;;(float (/ (int (* (rand) 1000)) 1000))
  (float (/ (random-int 200 999) 1000))
    )

(defn select-volume
  [player]
  (let [smoothness (volume-smoothness (get-melody-char-vol-smoothness (get-melody-char player)))
        last-volume (get-volume-for-event (get-last-melody-event player))
        vol-min (if last-volume (max (- last-volume smoothness) MIN-VOLUME) MIN-VOLUME)
        vol-max (if last-volume (min (+ last-volume smoothness) 1) 1)
        ]
    (if (not= last-volume 0)
      (select-volume-in-range vol-min vol-max)
      (select-random-volume)
      )
    ))

(defn select-ensemble-volume
  []
  (let [average-volume (get-average-volume)]
    (select-volume-in-range
     (if (<= average-volume 0.3) MIN-VOLUME (- average-volume 0.1)) ;; set range of volume to
     (if (> average-volume 0.9) 1 (+ average-volume 0.1))) ;; + or - 0.1 of average volume
    )
  )

(defn select-volume-similar-ensemble
  [player]
  (let [smoothness (volume-smoothness (get-melody-char-vol-smoothness (get-melody-char player)))
        last-volume (get-volume-for-event (get-last-melody-event player))
        vol-min (if last-volume (max (- last-volume smoothness) MIN-VOLUME) MIN-VOLUME)
        vol-max (if last-volume (min (+ last-volume smoothness) 1) 1)
        ]
    (if (not= last-volume 0)
      (select-volume-in-range vol-min vol-max)
      (select-ensemble-volume)
      )
    ))

(defn select-volume-for-next-note
  [player new-seg? event-time next-pitch]
  (let [player-action (get-behavior-action (get-behavior player))]
    (cond
     (not next-pitch) 0  ;; if no pitch (rest) set volume to 0
     (= player-action SIMILAR-ENSEMBLE)
     (if new-seg?
       (select-ensemble-volume)
       (select-volume-similar-ensemble player)
       )
     (= player-action CONTRAST-ENSEMBLE)
     (let [average-volume (get-average-volume)]
       (select-volume-in-range
        (if (< average-volume 0.5) 0.7 MIN-VOLUME) ;; set range of volume eithe 0.7 - 1 or
        (if (< average-volume 0.5) 1 0.3))) ;; MIN-VOLUME - 0.3 opposite of ensemble
     ;; else pick next melody note based only on players settings
     ;;  do not reference other players or ensemble
     :else (select-volume player) )
    )

  )
