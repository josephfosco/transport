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

(ns transport.rhythm
  (:require
   [transport.behaviors :refer [get-behavior-action-for-player]]
   [transport.ensemble-status :refer [get-average-note-dur-millis get-ensemble-mm]]
   [transport.melodychar :refer [get-melody-char-density]]
   [transport.players :refer :all]
   [transport.random :refer [add-probabilities random-dur random-int weighted-choice]]
   [transport.settings :refer [SIMILAR-ENSEMBLE]]
   [transport.util :refer :all]
   [overtone.live :refer [metronome]]
   ))

(def min-mm 40)
(def max-mm 200)

(def base-dur 1)
(def quarter-note 8)

(def NOTE-DURS
  [(int (* quarter-note (/ 1 8)))         ; 32nd note - 1
   (int (* quarter-note (/ 1 4)))         ; 16th note - 2
   (int (+ (* quarter-note (/ 1 4)) (* quarter-note (/ 1 8))))    ; dotted 16nd note - 3
   (int (* quarter-note (/ 1 2)))         ; 8th note - 4
   (int (+ (* quarter-note (/ 1 2)) (* quarter-note (/ 1 4))))    ; dotted 8th note - 6
   quarter-note                           ; quarter note - 8
   (+ quarter-note (/ quarter-note 2))    ; dotted quarter - 12
   (* quarter-note 2)                     ; half - 16
   (* quarter-note 3)                     ; dotted half - 24
   (* quarter-note 4)                     ; whole - 32
   (* quarter-note 8)                     ; double whole - 64
   ])

(def NOTE-DURS-BEATS (mapv #(/ (NOTE-DURS %1) quarter-note) (range 0 (count NOTE-DURS))))

(def NOTE-PROBS
  [2 8 5 15 10 15 10 15 10 5 5]
)

(def DENSITY-PROBS
  ;;  32   16   16.  8    8.   q    q.   h    h.   w    ww
  ;; [2    8    5    15   10   15   10   15   10   5    5   ]
  {0 [-999 -999 -999 -999 -999 -999 -999 0    25   40   40  ]
   1 [-999 -999 -999 -999 -999 0    0    10   20   40   10  ]
   2 [-999 -999 -999 0    0    15   0    20   20   20   0   ]
   3 [-999 0    -999 10   0    10   10   10   0    0    0   ]
   4 [0    0    0    5    0    10   5    0    0    0    0   ]
   6 [0    10   0    15   0    10   0    0    0    -5   -999]
   7 [0    10   0    15   10   0    0    -5   -5   -999 -999]
   8 [10   35   10   35   5    0    0    -10  -999 -999 -999]
   9 [25   40   25   35   0    0    0    -999 -999 -999 -999]
   }
  )

(defn note-dur-to-millis
  "Converts note-dur (in beats) to millis at the specified mm

   mm - the mm to compute note-dur with
   note-dur - note duration in beats"
  [mm note-dur]
  (int (+ 0.5 (* (* (/ 60.0 mm) note-dur)  1000)))   ;; round up or down
  )

(defn millis-to-note-dur
  "Converts millis to beats at the specified mm

   mm - the mm to compute note-dur with
   millis - note duration in millis"
  [mm millis]
  (/ millis (* (/ 60.0 mm)) 1000))


(defn note-dur-to-millis-player
  "Converts note-dur (in beats) to millis at the mm for player

   player - player to get mm for
   note-dur - note duration in beats"
  [player note-dur]
  (note-dur-to-millis (get-mm player) note-dur)
  )

(defn millis-to-note-dur-player
  [player millis]
  (millis-to-note-dur (get-mm player) millis))

(defn compute-mm-from-dur-info
  [millis beats]
  (println "rhythm.clj - compute-mm-from-dur-info millis:" millis "beats:" beats)
  (let [quarter-note-millis (+ (* millis (/ (- 1 beats) beats)) millis)
        ]
    (int (+ 0.5 (* (/ 1000 quarter-note-millis) 60)))) ;; round up or down
  )

(defn get-dur-millis
  "Returns the millis of this dur-info

   dur-info - duration info to get dur-beats from"
  [dur-info]
  (:dur-millis dur-info)
  )

(defn get-dur-beats
  "Returns the duration in beats of this dur-info

   dur-info - duration info to get dur-beats from"
  [dur-info]
  (:dur-note-dur dur-info)
  )

(defn select-mm
  ([] (random-int min-mm max-mm))
  ([player]
     (if (= (get-behavior-action-for-player player) SIMILAR-ENSEMBLE)
       (let [ensemble-mm (get-ensemble-mm)]
         (do
           (println "!!!!!!!!!")
           (print-msg "select-mm" "returning ensemble-mm: " ensemble-mm)
           (println "!!!!!!!!")
           ensemble-mm)
         )
       (random-int min-mm max-mm)
       )
     )
  )

(defn select-metronome-mm
  [mm]
  (metronome mm))

(defn select-metronome
  [player]
  (metronome (:mm player)))

(defn get-dur-info-for-mm-and-millis
  "Returns :dur-info map with
     :dur-note-dur = beats
     :dur-millis = beats converted to milliseconds

   mm - mm to use to determine beats
   millis - the number of millis to use in dur-info and
             convert to beats"
  [mm millis]
  {:dur-millis millis
   :dur-note-dur (millis-to-note-dur mm millis)}
  )

(defn get-dur-info-for-beats
  "Returns :dur-info map with
     :dur-note-dur = beats
     :dur-millis = beats converted to milliseconds

   player - player map to use to convert beats to milliseconds
   beats - the number of beats to use in dur-info and
             convert to milliseconds"
  [player beats]
  {:dur-millis (note-dur-to-millis-player player beats)
   :dur-note-dur beats}
  )

(defn adjust-note-prob
  " Finds the index of the rhythmic value closest to ensemble average duration,
    then adds 10 to that index's probability in NOTE-PROBS. It adds 5 to the
    probabilities of the values on either side of the index. If this
    is the first or last index, add 5 to the probability of the index
    either before or after the selected one.
    Returns the new NOTE-PROBS probability map.

    note-dur-millis - the dur (in millis) to match "
  [note-durs-millis]
  (let [index-closest-to-average (last (keep-indexed #(if (<= %2 (get-average-note-dur-millis)) %1) note-durs-millis))]
    (cond
     (or (= index-closest-to-average 0) (nil? index-closest-to-average))  ;; first index
     (add-probabilities NOTE-PROBS {0 10 1 5})
     (= index-closest-to-average (- (count note-durs-millis) 1))          ;; last index
     (add-probabilities NOTE-PROBS
                        {(- (count note-durs-millis) 1) 10
                         (- (count note-durs-millis) 2) 5})
     :else (add-probabilities NOTE-PROBS
                              {index-closest-to-average 10
                               (- index-closest-to-average 1) 5
                               (+ index-closest-to-average 1) 5}))
    )
  )

(defn- adjust-rhythmic-probabilities
  [player]
  (let [player-action (get-behavior-action-for-player player)
        note-durs-millis (map note-dur-to-millis (repeat (get-mm player)) NOTE-DURS-BEATS)
        adjusted-note-prob1 (if (= SIMILAR-ENSEMBLE player-action)
                             (adjust-note-prob note-durs-millis)
                             NOTE-PROBS)
        adjusted-note-prob2 (if-let [prob-adjust (get DENSITY-PROBS (get-melody-char-density (get-melody-char player)))]
                              (mapv + adjusted-note-prob1 prob-adjust)
                              adjusted-note-prob1)
        ;; make all probs < 0 be 0
        final-adjusted-note-prob (map #(if (< %1 0) 0 %1) adjusted-note-prob2)
        ]
    final-adjusted-note-prob
    )
  )

(defn create-note-dur
  []
  )

(defn next-note-dur
  "Returns :dur-info map for the next note

   player - player map to use when determining next note :dur-info"
  [player]
  (let [note-dur (weighted-choice (adjust-rhythmic-probabilities player))
        ]
    (if (nil? (get-mm player))
      (do
        (println "rhythm.clj - next-note-dur - mm is nil!!!!")
        (print-player player)
        )
      )
    {:dur-millis (note-dur-to-millis-player player (/ (NOTE-DURS note-dur) quarter-note))
     :dur-note-dur (/ (NOTE-DURS note-dur) quarter-note)}
    ))

;; (+ (tempm1 0) (beat-ms 1.5 (metro-bpm tempm1)))
;; takes some beat (0 in this case) and adds some number of beats to it
;; returns the number of milliseconds past the first beat (0 in this case)
