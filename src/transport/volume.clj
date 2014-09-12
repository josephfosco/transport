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
   [transport.behaviors :refer [get-behavior-ensemble-action-for-player]]
   [transport.ensemble-status :refer [get-average-volume]]
   [transport.melodychar :refer [get-melody-char-smoothness]]
   [transport.melodyevent :refer [get-volume-for-event]]
   [transport.players :refer :all]
   [transport.settings :refer :all]
   ))

(def volume-smoothness [9 8 7 6 5 4 3 2 1 0])

(defn select-volume-in-range
  "Returns a random float between lo-vol and hi-vol(exclusive).
   precision is only to .999

  lo-vol - lowest value that can be returned
  hi-vol - highest value that can be returned(exclusive)"
  [lo-vol hi-vol]
  (float (/ (int (* (+ (rand (- hi-vol lo-vol)) lo-vol) 1000)) 1000))
  )

(defn- select-random-volume
  []
  (float (/ (int (* (rand) 1000)) 1000))
    )

(defn select-volume
  [player]
  (let [smoothness (volume-smoothness (get-melody-char-smoothness (get-melody-char player)))
        last-volume (get-volume-for-event (get-last-melody-event player))
        vol-min (if last-volume (max (- last-volume (* smoothness 0.05)) 0) 0)
        vol-max (if last-volume (min (+ last-volume (* smoothness 0.05)) 1) 1)
        ]
    (if (not= last-volume 0)
      (select-volume-in-range vol-min vol-max)
      (select-random-volume)
      )
    ))

(defn select-volume-for-next-note
  [player event-time next-pitch]
  (let [behavior-ensemble-action (get-behavior-ensemble-action-for-player player)]
    (cond
     (not next-pitch) 0  ;; if no pitch (rest) set volume to 0
     (= behavior-ensemble-action SIMILAR)
     (let [average-volume (get-average-volume)]
       (select-volume-in-range
        (if (< average-volume 0.1) 0 (- average-volume 0.1)) ;; set range of volume to
        (if (> average-volume 0.9) 1 (+ average-volume 0.1)))) ;; + or - 0.1 of average volume
     (= behavior-ensemble-action CONTRAST)
     (let [average-volume (get-average-volume)]
       (select-volume-in-range
        (if (< average-volume 0.5) 0.7 0) ;; set range of volume eithe 0.7 - 1 or
        (if (< average-volume 0.5) 1 0.3))) ;; 0 - 0.3 opposite of ensemble
     ;; else pick next melody note based only on players settings
     ;;  do not reference other players or ensemble
     :else (select-volume player) )
    )

  )
