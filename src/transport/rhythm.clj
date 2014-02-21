;    Copyright (C) 2013  Joseph Fosco. All Rights Reserved
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
  (:use
   [transport.ensemble-status :only [get-average-note-dur-millis]]
   [transport.players]
   [transport.random :only [random-dur random-int weighted-choice]]
   [transport.settings :only [COMPLEMENT]]
   [overtone.live :only [metronome]]
   ))

(def min-mm 40)
(def max-mm 200)

(def base-dur 1)
(def quarter-note 8)

(def NOTE-DURS
  [(int (* quarter-note (/ 1 8)))         ; 32nd note - 1
   (int (* quarter-note (/ 1 4)))         ; 16th note - 2
   (int (+ (* quarter-note (/ 1 4)) (* quarter-note (/ 1 8))))    ; dotted 32nd note - 3
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
  {0 {0 -999 1 -999 2 -999 3 -999 4 -999 5 -999 6 -999 8 25 9 40 10 40}
   8 {1 35 3 35 4 25 8 -999 9 -999 10 -999}
   9 {0 25 1 40 2 25 3 35 7 -999 8 -999 9 -999 10 -999}
   }
  )

(defn note-dur-to-millis
  "Converts note-dur (in beats) to millis at the mm for player

   player - player to get mm for
   note-dur - note duration in beats"
  [player note-dur]
  (int (* (* (/ 60.0 (:mm player)) note-dur)  1000))
  )

(defn millis-to-note-dur
  [player millis]
  (/ millis (* (/ 60.0 (:mm player)) 1000)))

(defn get-beats
  "Returns the duration in beats of this dur-info

   dur-info - duration info to get dur-beats from"
  [dur-info]
  (:dur-note-dur dur-info)
  )

(defn select-mm
  [player]
  (random-int min-mm max-mm)
  )

(defn select-metronome
  [player]
  (metronome (:mm player)))

(defn get-dur-info-for-beats
  "Returns :dur-info map with
     :dur-note-dur = beats
     :dur-millis = beats converted to milliseconds

   player - player map to use to convert beats to milliseconds
   beats - the number of beats to use in dur-info and
             convert to milliseconds"
  [player beats]
  {:dur-millis (note-dur-to-millis player (* quarter-note beats))
   :dur-note-dur beats}
  )

(defn add-probabilities
  "Adds to probabilities in prob-vector. This function does not
   do any error checking. If multiple values are specified for
   a single index, all the values will be added to the value in
   prob-vector.

  prob-vector - vector of probabilities to add probabilities to
  prob-to-add-map - a map of probabilities to add where each entry is
                    key - the index to add to
                    value - the amount to add to the probability"
  [prob-vector prob-to-add-map]
  (loop [cur-prob-vector prob-vector
         prob-indexes (keys prob-to-add-map)
         prob-values (vals prob-to-add-map)]
    (if (= (count prob-indexes) 0)
      cur-prob-vector
      (recur (assoc cur-prob-vector (first prob-indexes) (+ (first prob-values) (nth cur-prob-vector (first prob-indexes))))
             (next prob-indexes)
             (next prob-values)))
    )
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

(defn adjust-rhythmic-probabilities
  [player]
  (let [ensemble-action (get-behavior-ensemble-action player)
        note-durs-millis (map note-dur-to-millis (repeat player) NOTE-DURS-BEATS)
        adjusted-note-prob1 (if (= COMPLEMENT ensemble-action)
                             (adjust-note-prob note-durs-millis)
                             NOTE-PROBS)
        adjusted-note-prob2 (if-let [prob-adjust (get DENSITY-PROBS (get-melody-density-char player))]
                              (add-probabilities adjusted-note-prob1 prob-adjust)
                              adjusted-note-prob1)
        final-adjusted-note-prob (map #(if (< %1 0) 0 %1) adjusted-note-prob2)
        ]
    ;; (println "adjusted probabilities:" final-adjusted-note-prob "mm:" (get-mm player))
    final-adjusted-note-prob
    )
  )

(defn next-note-dur
  "Returns :dur-info map for the next note

   player - player map to use when determining next note :dur-info"
  [player]
  (let [note-dur (weighted-choice ( adjust-rhythmic-probabilities player))
        ]
    {:dur-millis (note-dur-to-millis player (/ (NOTE-DURS note-dur) quarter-note))
     :dur-note-dur (/ (NOTE-DURS note-dur) quarter-note)}
    ))

;; (+ (tempm1 0) (beat-ms 1.5 (metro-bpm tempm1)))
;; takes some beat (0 in this case) and adds some number of beats to it
;; returns the number of milliseconds past the first beat (0 in this case)
