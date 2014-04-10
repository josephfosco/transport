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

(ns transport.pitch
  (:use
   [transport.behaviors :only [get-behavior-action-for-player]]
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
  (let [semitones-above-root (mod (- pitch (get-key player)) OCTAVE)]
    (.indexOf (get SCALES (get-scale player)) semitones-above-root)
    )
    )

(defn get-nearest-scale-degree
  "Returns nearest scale degree that is closest to pitch in players key and scale
   value returned is index to players scale vector from SCALES

   player - player to use key and scale from
   pitch - pitch to find nearest scale degree in players key and scale"
  [player pitch]
  (let [semitones-above-root (mod (- pitch (get-key player)) OCTAVE)
        octave-scale (conj (get SCALES (get-scale player)) OCTAVE) ;; add OCTAVE interval at end in case pitch is between last interval and octave
        ]

    (loop [i 0
           scale octave-scale
           ]
      (let [interval-1 (nth scale 0)
            interval-2 (nth scale 1)
            ]

        (if (and (<= semitones-above-root interval-2) (>= semitones-above-root interval-1))
          (cond (< (- semitones-above-root interval-1)
                   (- interval-2 semitones-above-root)) i        ;;  closer or = to lower interval
                (> (- semitones-above-root interval-1)
                   (- interval-2 semitones-above-root))          ;;  closer or = to higher interval
                (if (= (count scale) 2) 0 (inc i))               ;;  if at top only return 0 (not octave)
                :else i                                          ;;  equal distance from each interval
                   )
          (recur (inc i) (subvec scale 1))
          ))
      )
    )
  )

(defn get-step-up-in-scale
  "Returns the pitch 1 step up in the players scale and key
   Will return 0 if pitch is not in scale and key

   player - the player to select key and scale from
   pitch - the pitch to go 1 step above"
  [player pitch]
  (let [player-scale (get SCALES (get-scale player))
        tst-scale-degree (get-scale-degree player pitch)
        scale-degree (if (not= tst-scale-degree -1) tst-scale-degree (get-nearest-scale-degree player pitch))
        ]
    (if (= scale-degree (- (count player-scale) 1))         ;; if at top of scale
      (+ pitch (- OCTAVE (get player-scale scale-degree)))  ;; return root above pitch
      (+ pitch (- (get player-scale (+ scale-degree 1))
                  (get player-scale scale-degree)))         ;; else return pitch 1 scale degree up
      )
    )
  )

(defn get-step-down-in-scale
  "Returns the pitch 1 step down in the players scale and key
   Will return 0 if pitch is not in scale and key

   player - the player to select key and scale from
   pitch - the pitch to go 1 step below"
  [player pitch]
  (let [player-scale (get SCALES (get-scale player))
        tst-scale-degree (get-scale-degree player pitch)
        scale-degree (if (not= tst-scale-degree -1) tst-scale-degree (get-nearest-scale-degree player pitch))
        ]
    (if (= scale-degree 0)     ;; if at bottom of scale
      (- pitch
         (- OCTAVE (get player-scale (- (count player-scale) 1))))  ;; return top of scale below pitch
      (- pitch (- (get player-scale scale-degree)
                  (get player-scale (- scale-degree 1))))           ;; else return pitch 1 scale degree down
      )
    )
  )

(defn select-random-key
  "Returns a randow number between 1- 11
   to represent a key. 0=C"
  []
  (random-int 0 11))

(defn select-key
  "returns a randow number between 1- 11
   to represent a key. 0=C"
  [player]
  (random-int 0 11))

(defn select-direction
  [player]
  ;; if at the begining of a segment, play a random note
  ;; else pick direction for this note
  (if (= (get-last-melody-note player) nil)
    RANDOM-NOTE
    (let [rand-dir (rand)]
      (if (<= rand-dir 0.45)
        DESCEND
        (if ( <= rand-dir 0.9)
          ASCEND
          REPEAT-NOTE))
      )))

(defn select-random-scale
  "returns a scale"
  []
  ( nth (keys SCALE) (random-int 0 (- (count SCALE) 1))))

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
  (let [rand-rounded (read-string (format "%.1f" (* (rand) 10)))] ;; scales rand to int + 1 decimal place (0 - 9.9)
    (if (>  rand-rounded (get-melody-smoothness-char player)) STEP SKIP))
  )

(defn dir-ascend
  [player]
  (if (= (choose-step-or-skip player) STEP)
    (get-step-up-in-scale player (get-last-melody-note player))
    (let [prev-note (get-last-melody-note player)
          lo (or (if prev-note (+ prev-note 1) nil) (get-lo-range player)) ]
      (get-scale-pitch-in-range player :lo-range (or (if prev-note (+ prev-note 1) nil) (get-lo-range player)))))
  )

(defn dir-descend
  [player]
  (if (= (choose-step-or-skip player) STEP)
    (get-step-down-in-scale player (get-last-melody-note player))
    (let [prev-note (get-last-melody-note player)]
      (get-scale-pitch-in-range player :hi-range (or (if prev-note (- prev-note 1) nil) (get-hi-range player)))))
  )

(defn next-pitch-ignore
  [player]
  (let [direction (select-direction player)]
    (cond
     (= direction ASCEND) (dir-ascend player)
     (= direction DESCEND) ( dir-descend player)
     (= direction RANDOM-NOTE) (random-int (get-lo-range player) (get-hi-range player))
     :else (get-last-melody-note player)))  )

(defn next-pitch-complement
  [player]
  (next-pitch-ignore player)
  )

(defn next-pitch-contrast
  [player]
  (next-pitch-ignore player)
  )

(defn next-pitch
  [player & {:keys [note-dir]
             :or {note-dir nil}}]
  (let [player-behavior-action (get-behavior-action-for-player player)]
    (cond
     (= player-behavior-action COMPLEMENT) (next-pitch-complement player)
     (= player-behavior-action CONTRAST) (next-pitch-contrast player)
     (= player-behavior-action IGNORE) (next-pitch-ignore player)
     :else (println "pitch.clj - next-pitch - ERROR - Invalid behavior-action:" player-behavior-action))) )
