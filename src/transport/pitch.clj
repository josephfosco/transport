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
   [transport.players]
   [transport.instrument :only [get-hi-range get-lo-range get-instrument-range]]
   [transport.random :only [random-pitch random-int]]
   [transport.settings]
   ))

(def SCALES {})
(def DESCEND 0)
(def ASCEND 1)
(def REPEAT-NOTE 3)
(def RANDOM-NOTE 4)
(def STEP 0)
(def SKIP 1)
(def OCTAVE 12)

(defn convert-scale
  "Convert a list that represents a scale as intervals between adjacent notes
   to a list that represents a scale as the interval from the starting note.
   Returns the new representation."
  [scale & new-scale]
  (def SCALES {})
  (let [rtn-scale (or new-scale [0])]
    (if (= (count scale) 1)
      new-scale
      (recur (rest scale) (conj rtn-scale (+ (first scale) (last rtn-scale))))
      )))

(defn load-scales
  []
  (doseq [scale-key (keys SCALE)]
    (def SCALES (assoc SCALES
                  scale-key
                  (convert-scale (scale-key SCALE))))))

(defn get-scale-degree-semitones
  "Returns the number of semitones from tonic that
   scale degree is in player's scale"
  [player scale-degree]
  (- (nth ((:scale player) SCALES) scale-degree)
  ))

(defn get-scale-pitch-in-range
  "return a pitch from player scale that is within player range
   if :lo-range or :hi-range are specfied use these instead of the
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

(defn get-scale-degree
  "Returns the scale degree (zero-based) of pitch in player's scale and key.
   Returns -1 if pitch is not in scale

   player - the player this pitch is from
   pitch - the pitch to determine the scale degree of"
  [player pitch]
  (let [semitones-above-root (mod (- pitch (get-key player)) 12)]
    (.indexOf (get SCALES (get-scale player)) semitones-above-root)
       )
  )

(defn get-step-up-in-scale
  "Returns the pitch 1 step up in the players scale and key
   Will return 0 if pitch is not in scale and key

   player - the player to select key and scale from
   pitch - the pitch to go 1 step above"
  [player pitch]
  (println "step UP")
  (let [player-scale (get SCALES (get-scale player))
        tst-scale-degree (get-scale-degree player pitch)
        scale-degree (if (not= tst-scale-degree -1) tst-scale-degree 0) ;; in case pitch is not in scale & key
        ]
    (if (= scale-degree (- (count player-scale) 1))     ;; if at top of scale
      (+ pitch (- 12 (get player-scale scale-degree)))  ;; return root above pitch
      (+ pitch (- (get player-scale (+ scale-degree 1))
                  (get player-scale scale-degree)))     ;; else return pitch 1 scale degree up
      )
    )
  )

(defn get-step-down-in-scale
  "Returns the pitch 1 step down in the players scale and key
   Will return 0 if pitch is not in scale and key

   player - the player to select key and scale from
   pitch - the pitch to go 1 step below"
  [player pitch]
  (println "step Down")
  (let [player-scale (get SCALES (get-scale player))
        tst-scale-degree (get-scale-degree player pitch)
        scale-degree (if (not= tst-scale-degree -1) tst-scale-degree 0) ;; in case pitch is not in scale & key
        ]
    (if (= scale-degree 0)     ;; if at bottom of scale
      (- pitch
         (- 12 (get player-scale (- (count player-scale) 1))))  ;; return top of scale below pitch
      (- pitch (- (get player-scale scale-degree)
                  (get player-scale (- scale-degree 1))))       ;; else return pitch 1 scale degree down
      )
    )
  )

(defn select-key
  "returns a randow number between 1- 11
   to represent a key. 0=C"
  [player]
  (random-int 0 11))

(defn select-direction
  [player]
  ;; if at the begining of a segment, play a random note
  ;; else pick direction for this note
  (println "LAST MELODY NOTE: " (get-last-melody-note player))
  (if (= (get-last-melody-note player) nil)
    RANDOM-NOTE
    (let [rand-dir (rand)]
      (if (<= rand-dir 0.45)
        DESCEND
        (if ( <= rand-dir 0.9)
          ASCEND
          REPEAT-NOTE))
      )))

(defn select-scale
  "returns a scale"
  [player]
  ( nth (keys SCALE) (random-int 0 (- (count SCALE) 1))))

(defn select-scale-degree
  [player]
  (random-int
   0
   (- (count ((:scale player) SCALES)) 1)))  ; number of pitches in the instruments scale

(defn choose-step-or-skip
  "Returns STEP or SKIP for next note for player

   player - player to get STEP or SKIP for"
  [player]
  (let [rand-tst (read-string (format "%.1f" (* (rand) 10)))]
    (println "**** rand-tst ****: " rand-tst)
    (if (>  rand-tst (get-melody-smoothness player)) STEP SKIP))
  )

(defn dir-ascend
  [player]
  (println "dir-ascend")
  (if (= (choose-step-or-skip player) STEP)
    (get-step-up-in-scale player (get-last-melody-note player))
    (let [prev-note (get-last-melody-note player)
          lo (or (if prev-note (+ prev-note 1) nil) (get-lo-range player)) ]
      (get-scale-pitch-in-range player :lo-range (or (if prev-note (+ prev-note 1) nil) (get-lo-range player)))))
  )

(defn dir-descend
  [player]
  (println "dir-decend")
  (if (= (choose-step-or-skip player) STEP)
    (get-step-down-in-scale player (get-last-melody-note player))
    (let [prev-note (get-last-melody-note player)]
      (get-scale-pitch-in-range player :hi-range (or (if prev-note (- prev-note 1) nil) (get-hi-range player)))))
  )

(declare next-pitch-ignore)
(defn next-pitch-follow
  [player]
  ;; if following, ignore key and scale and mach pitch as accurately as you can
  (next-pitch-ignore player)
 )

(defn next-pitch-complement
  [player]
  (next-pitch-ignore player)
  )

(defn next-pitch-contrast
  [player]
  (next-pitch-ignore player)
  )

(defn next-pitch-ignore
  [player]
  (let [direction (select-direction player)]
    (println "direction: " direction)
    (cond
     (= direction ASCEND) (dir-ascend player)
     (= direction DESCEND) ( dir-descend player)
     (= direction RANDOM-NOTE) (random-int (get-lo-range player) (get-hi-range player))
     :else (get-last-melody-note player)))  )

(defn next-pitch
  [player & {:keys [note-dir]
           :or {note-dir nil}}]
  (cond
   (= get-behavior-action FOLLOW) (next-pitch-follow player)
   (= get-behavior-action COMPLEMENT) (next-pitch-complement player)
   (= get-behavior-action CONTRAST) (next-pitch-contrast player)
   :else (next-pitch-ignore player)) )
