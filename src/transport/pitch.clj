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

(ns transport.pitch
  (:use
   [overtone.music.pitch :only [SCALE]]
   [transport.instrument :only [get-hi-range get-lo-range get-instrument-range]]
   [transport.random :only [random-pitch random-int]]))

(def SCALES {})
(def DESCEND 0)
(def ASCEND 1)
(def SAME-NOTE 3)
(def OCTAVE 12)

(defn prev-melody-note [player]
  (if (not= (:melody player) [])
    (:note (last (:melody player)))
    nil))

(defn get-scale-degree-semitones
  "Returns the number of semitones from tonic that
   scale degree is in player's scale"
  [player scale-degree]
  (- (nth ((:scale player) SCALES) scale-degree)
  ))

(defn get-scale-pitch-in-range
  "return a pitch from player scale that is within player range
   if :lo-pitch or :hi-pitch are specfied use these instead of the
   values indicated by player."
  [player & {:keys [lo-range hi-range]
             :or {lo-range (get-lo-range player)
                  hi-range (get-hi-range player)}} ]
  (let [lo-pitch lo-range
        hi-pitch hi-range
        player-key (:key player)
        rand-pitch (random-int lo-pitch hi-pitch)
        rand-octave (int (/ rand-pitch OCTAVE)) ;; octave multiplier for rand-pitch
        semitones-above-c (mod rand-pitch OCTAVE)
        semitones-above-tonic (mod (+ semitones-above-c (- OCTAVE player-key)) OCTAVE)
        scale-degree-in-range (last
                               (for [s ((:scale player) SCALES)
                                     :while (<=
                                             s
                                             semitones-above-tonic)]
                                 s))
        ;; now place scale degree in correct octave
        possible-rtn-pitch (+ (* rand-octave OCTAVE) scale-degree-in-range player-key)
        ]
    (cond
     (<= lo-pitch possible-rtn-pitch hi-pitch) ;; if new-pitch within range
     possible-rtn-pitch                          ;; return it
     (<= lo-pitch (- possible-rtn-pitch OCTAVE) hi-pitch);;  try octve lower
     (- possible-rtn-pitch OCTAVE)
     :else lo-pitch )))                                ;; else return lo-pitch

(defn select-key
  "returns a randow number between 1- 11
   to represent a key. 0=C"
  [player]
  (random-int 0 11))

(defn select-direction [player]
  (let [rand-dir (rand)]
    (if (<= rand-dir 0.45)
      DESCEND
      (if ( <= rand-dir 0.9)
          ASCEND
          SAME-NOTE))
    ))

(defn select-scale
  "returns a scale"
  [player]
  ( nth (keys SCALE) (random-int 0 (- (count SCALE) 1))))

(defn select-scale-degree [player]
  (random-int
   0
   (- (count ((:scale player) SCALES)) 1)))  ; number of pitches in the instruments scale

(defn dir-ascend [player]
  (let [prev-note (prev-melody-note player)
        lo (or (if prev-note (+ prev-note 1) nil) (get-lo-range player)) ]
    (get-scale-pitch-in-range player :lo-range (or (if prev-note (+ prev-note 1) nil) (get-lo-range player))))
  )

(defn dir-descend [player]
  (let [prev-note (prev-melody-note player)]
    (get-scale-pitch-in-range player :hi-range (or (if prev-note (- prev-note 1) nil) (get-hi-range player))))
  )

(defn next-pitch [player]
  (let [direction (select-direction player)]
    (cond
     (= direction ASCEND) (dir-ascend player)
     (= direction DESCEND) ( dir-descend player)
     :else (let [prev-melody (prev-melody-note player)]
             (if (not= prev-melody nil)
               prev-melody
               (random-int (get-lo-range player) (get-hi-range player)))))))

(defn convert-scale
  "Convert a list that represents a scale as intervals between adjacent notes
   to a list that represents a scalea the interval from the starting note.
   Returns the new representation."
  [scale & new-scale]
  (def SCALES {})
  (let [rtn-scale (or new-scale [0])]
    (if (= (count scale) 1)
      new-scale
      (recur (rest scale) (conj rtn-scale (+ (first scale) (last rtn-scale))))
      )))

(defn load-scales []
  (doseq [scale-key (keys SCALE)]
    (def SCALES (assoc SCALES
                  scale-key
                  (convert-scale (scale-key SCALE))))))
