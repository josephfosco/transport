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
   [transport.constants :refer :all]
   [transport.ensemble-status :refer [get-average-playing-volume get-ensemble-trend-volume get-volume-trend-diff]]
   [transport.melodychar :refer [get-melody-char-vol-smoothness]]
   [transport.melodyevent :refer [get-volume-for-event]]
   [transport.players :refer :all]
   [transport.random :refer [random-int]]
   [transport.settings :refer [ensemble-volume-change-threshold min-volume]]
   [transport.util.util-constants :refer [DECREASING INCREASING STEADY]]
   [transport.util.utils :refer [print-msg]]
   ))

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
  (let [last-volume (get-volume-for-event (get-last-melody-event player))]
    (if (or (= last-volume 0) (nil? last-volume))
      (select-random-volume)
      (let [smoothness (volume-smoothness (get-melody-char-vol-smoothness (get-melody-char player)))
            vol-min (max (- last-volume (* (/ smoothness 2) 0.1)) @min-volume)
            vol-max (min (+ last-volume (* (/ smoothness 2) 0.1)) 1)
            ]
        (select-volume-in-range vol-min vol-max)
        )))
  )

(defn select-ensemble-volume
  []
  (let [average-volume (get-average-playing-volume)]
    (cond (= (get-ensemble-trend-volume) STEADY)
          (select-volume-in-range
           (max @min-volume (- average-volume 0.05)) ;; set range of volume to
           (min 1 (+ average-volume 0.05)))          ;; + or - 0.05 of average volume
          (= (get-ensemble-trend-volume) INCREASING)
          (select-volume-in-range
           (min 1 (max @min-volume (+ average-volume 0.05)))
           (min 1 (+ average-volume 0.2)))
          :else
          (select-volume-in-range
           (max @min-volume (- average-volume 0.2))
           (max @min-volume (- average-volume 0.05)))
          )
    )
  )

(defn select-volume-similar-ensemble
  [player]
  (let [last-volume (get-volume-for-event (get-last-melody-event player))]
    (cond (or (= last-volume 0) (nil? last-volume))
          (select-ensemble-volume)
          (= (get-ensemble-trend-volume) INCREASING)
          (min (+ (select-volume player)
                  (max (- (get-volume-trend-diff) (* 0.5 @ensemble-volume-change-threshold)) @min-volume))
               1)
          (= (get-ensemble-trend-volume) DECREASING)
          (max (- (select-volume player)
                  (min (- (get-volume-trend-diff) (* 0.5 @ensemble-volume-change-threshold)) 1))
               @min-volume)
          :else
          (select-volume player)
      ))
  )

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
     (let [average-volume (get-average-playing-volume)]
       (select-volume-in-range
        (if (< average-volume 0.5) 0.7 @min-volume) ;; set range of volume eithe 0.7 - 1 or
        (if (< average-volume 0.5) 1 0.3))) ;; min-volume - 0.3 opposite of ensemble
     ;; else pick next melody note based only on players settings
     ;;  do not reference other players or ensemble
     :else (select-volume player) )
    )

  )
