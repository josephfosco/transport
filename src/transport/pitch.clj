;    Copyright (C) 2013-2015  Joseph Fosco. All Rights Reserved
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
  (:require
   [overtone.live :refer [MIDI-RANGE]]
   [transport.behavior :refer [get-behavior-action]]
   [transport.ensemble-status :refer [get-ensemble-key-for-player]]
   [overtone.music.pitch :refer [SCALE]]
   [transport.instrument :refer [get-instrument-range-hi get-instrument-range-lo]]
   [transport.melodychar :refer [get-melody-char-range get-melody-char-range-hi get-melody-char-range-lo get-melody-char-pitch-smoothness]]
   [transport.players :refer :all]
   [transport.random :refer [random-pitch random-int]]
   [transport.util.constants :refer :all]
   )
  (:import transport.behavior.Behavior)
  )

(def SCALES (atom '()))
(def DESCEND 0)
(def ASCEND 1)
(def REPEAT-NOTE 3)
(def RANDOM-NOTE 4)
(def STEP 0)
(def SKIP 1)

(defn convert-scale
  "Convert a list that represents a scale as intervals between adjacent notes
   to a list that represents a scale as the interval from the starting note.
   Returns the new representation."
  [scale & new-scale]
  (let [rtn-scale (or new-scale [0])]
    (if (= (count scale) 1)
      new-scale
      (recur (rest scale) (conj rtn-scale (+ (first scale) (last rtn-scale))))
      )))

(defn load-scales
  []
  (reset! SCALES '())
  (doseq [scale-key (keys SCALE)]
    (reset! SCALES (conj @SCALES
                         (convert-scale (scale-key SCALE)))))
  (reset! SCALES (distinct @SCALES))
  )

(defn get-scale-degree-semitones
  "Returns the number of semitones from tonic that
   scale degree is in player's scale"
  [player scale-degree]
  (- (nth (:scale player) scale-degree)
  ))

(defn get-scale-pitch-in-range
  "return a pitch from player scale that is within player melody-char range
   if :lo-range or :hi-range are specfied use these instead of the
   values indicated by player."
  [player & {:keys [lo-range hi-range]
             :or {lo-range (get-melody-char-range-lo (get-melody-char player))
                  hi-range (get-melody-char-range-hi (get-melody-char player))}} ]
;;  (println "pitch.clj - get-scale-pitch-in-range lo:" lo-range "hi:" hi-range)
  (let [player-key (get-key player)
        rand-pitch (random-int lo-range hi-range)
        rand-octave (int (/ rand-pitch OCTAVE)) ;; octave multiplier for rand-pitch
        semitones-above-c (mod rand-pitch OCTAVE)
        semitones-above-tonic (mod (+ semitones-above-c (- OCTAVE player-key)) OCTAVE)
        scale-degree-in-range (last
                               (for [s (:scale player)
                                     :while (<=
                                             s
                                             semitones-above-tonic)]
                                 s))
        ;; now place scale degree in correct octave
        possible-rtn-pitch (+ (* rand-octave OCTAVE) scale-degree-in-range player-key)
        ]
    (cond
     (<= lo-range possible-rtn-pitch hi-range) ;; if new-pitch within range
     possible-rtn-pitch                          ;; return it
     (<= lo-range (- possible-rtn-pitch OCTAVE) hi-range);;  try octve lower
     (- possible-rtn-pitch OCTAVE)
     :else lo-range )))                                ;; else return lo-range

(defn get-scale-degree
  "Returns the scale degree (zero-based) of pitch in player's scale and key.
   Returns -1 if pitch is not in scale

   player - the player this pitch is from
   pitch - the pitch to determine the scale degree of"
  [player pitch]
  (let [semitones-above-root (mod (- pitch (get-key player)) OCTAVE)]
    (.indexOf (get-scale player) semitones-above-root)
    )
    )

(defn get-nearest-scale-degree
  "Returns nearest scale degree that is closest to pitch in players key and scale
   value returned is index to players scale vector from SCALES

   player - player to use key and scale from
   pitch - pitch to find nearest scale degree in players key and scale"
  [player pitch]
  (let [semitones-above-root (mod (- pitch (get-key player)) OCTAVE)
        octave-scale (conj (get-scale player) OCTAVE) ;; add OCTAVE interval at end in case pitch is between last interval and octave
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

   player - the player to select key and scale from
   pitch - the pitch to go 1 step above"
  [player pitch]
  (let [player-scale (get-scale player)
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
  "Returns the pitch 1 step down in the players scale and key or
   a negative number if there are no further pitches below
   in the players scale and key

   player - the player to select key and scale from
   pitch - the pitch to go 1 step below"
  [player pitch]
  (let [player-scale (get-scale player)
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
  "Returns a randow number between 0 - 11
   to represent a key. 0=C"
  []
  (rand-int 12))

(defn select-key
  "returns a randow number between 0 - 11
   to represent a key. 0=C"
  [player]
  (if (= (get-behavior-action (get-behavior player)) SIMILAR-ENSEMBLE)  ;; if SIMILARng ensemble
    (get-ensemble-key-for-player player)  ;; get key from ensemble else
    (rand-int 12))) ;; return random key

(defn select-direction
  [player]
  ;; if at the begining of a segment, play a random note
  ;; else pick direction for this note
  (let [last-note (get-last-melody-note player)]
    (cond (=  last-note nil) RANDOM-NOTE
          (<= last-note (get-instrument-range-lo (get-instrument-info player))) ASCEND
          (>= last-note (get-instrument-range-hi (get-instrument-info player))) DESCEND
          :else
          (let [rand-dir (rand)]
            (cond (<= rand-dir 0.45) DESCEND
                  (<= rand-dir 0.9) ASCEND
                  :else REPEAT-NOTE)
            )))
  )

(defn select-random-scale
  "returns a scale"
  []
  (nth @SCALES (rand-int (count @SCALES)))
  )

(defn select-scale
  "returns a scale"
  [player]
  (nth @SCALES (rand-int (count @SCALES)))
  )

(defn select-scale-degree
  [player]
  (random-int
   0
   (- (count (:scale player)) 1))) ; number of pitches in the instruments scale

(defn choose-step-or-skip
  "Returns STEP or SKIP for next note for player

   player - player to get STEP or SKIP for"
  [player]
  (let [rand-rounded (read-string (format "%.1f" (* (rand) 10)))] ;; scales rand to int + 1 decimal place (0 - 9.9)
    (if (>  rand-rounded (get-melody-char-pitch-smoothness (get-melody-char player))) STEP SKIP)
    )
  )

(defn check-note-in-range
  "Checks if note is in the melody range of player.
   Returns note if it is within range, else returns -1"
  [player note]
  (if (and (<= (first MIDI-RANGE) (get-melody-char-range-lo (get-melody-char player)) note)
           (<= note (get-melody-char-range-hi (get-melody-char player)) (last MIDI-RANGE)))
    note
    -1
    )
  )

(defn dir-ascend
  [player]
  (let [rtn-note (if (= (choose-step-or-skip player) STEP)
                   (get-step-up-in-scale player (get-last-melody-note player))
                   (let [prev-note (get-last-melody-note player)
                         melody-lo-range (get-melody-char-range-lo (get-melody-char player))
                         ]
                     (get-scale-pitch-in-range player
                                               :lo-range
                                               (or (if (and prev-note (>= prev-note melody-lo-range))
                                                     (inc prev-note)
                                                     nil)
                                                   melody-lo-range
                                                   )
                                               )
                     )
                   )]
    rtn-note
    )
  )

(defn dir-descend
  "Returns a pitch in player's key, scale and melody range
     lower than the previous pitch played.
     If there is no pitch that meets these requirements, returns -1

     player - the player to find the pitch for"
  [player]
  (let [prev-note (get-last-melody-note player)
        rtn-note (if (= (choose-step-or-skip player) STEP)
                   (let [step-down (get-step-down-in-scale player (get-last-melody-note player))
                         ]
                     (if (>= step-down (get-melody-char-range-lo (get-melody-char player)))
                       step-down
                       prev-note
                       )
                     )
                   (let [hi-range (or (if (and
                                           prev-note
                                           (<= prev-note (get-melody-char-range-hi (get-melody-char player))))
                                        (- prev-note 1)
                                        nil)
                                      (get-melody-char-range-hi (get-melody-char player)))
                         ]
                     (if (> hi-range (get-melody-char-range-lo (get-melody-char player)))
                            (get-scale-pitch-in-range player :hi-range hi-range)
                            prev-note
                            )
                     )
                   )
        ]
    rtn-note
    )
  )

(defn next-pitch-ignore
  [player]
  (let [direction (select-direction player)
        last-pitch (get-last-melody-note player)
        next-pitch (cond
                    (nil? last-pitch) (get-scale-pitch-in-range player)
                    (= direction ASCEND) (dir-ascend player)
                    (= direction DESCEND) (dir-descend player)
                    (= direction REPEAT-NOTE) (get-last-melody-note player)
                    :else (get-scale-pitch-in-range player)
                    )
        ]
    ;; if no pitch is available in direction selected, return a random pitch in melody range
    (if (= next-pitch (check-note-in-range player next-pitch))
      next-pitch
      (do
;;        (print-msg "next-pitch-ignore"  "CHOOSING NEW PITCH player-id: " (get-player-id player) " pitch: " next-pitch " range: " (get-melody-char-range-lo (get-melody-char player)) " " (get-melody-char-range-hi (get-melody-char player)) " key: " (get-key player) " scale: " (get-scale player))
        (get-scale-pitch-in-range player)
        )
      )
    )  )

(defn next-pitch-similar
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
  (let [player-behavior-action (get-behavior-action (get-behavior player))
        ]
    (cond
     (= player-behavior-action SIMILAR-PLAYER) (next-pitch-similar player)
     (= player-behavior-action CONTRAST-PLAYER) (next-pitch-contrast player)
     :else (next-pitch-ignore player)
     )) )
