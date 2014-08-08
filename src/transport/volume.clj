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
   [transport.melodychar :refer [get-melody-char-smoothness]]
   [transport.melodyevent :refer [get-volume-for-event]]
   [transport.players :refer :all]
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
