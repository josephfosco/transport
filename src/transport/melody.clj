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

(ns transport.melody
  (:require
   [overtone.live :refer [MIDI-RANGE]]
   [transport.behaviors :refer [get-behavior-action-for-player get-behavior-player-id-for-player]]
   [transport.pitch :refer [get-scale-degree next-pitch]]
   [transport.ensemble-status :refer [get-average-density get-ensemble-density get-rest-probability]]
   [transport.instrument :refer [get-hi-range get-lo-range]]
   [transport.melodychar :refer [get-melody-char-continuity get-melody-char-density get-melody-char-range get-melody-char-smoothness]]
   [transport.melodyevent :refer [create-melody-event get-dur-info-for-event get-follow-note-for-event get-instrument-info-for-event get-seg-num-for-event]]
   [transport.messages :refer :all]
   [transport.message-processor :refer [register-listener]]
   [transport.players :refer :all]
   [transport.random :refer [random-int weighted-choice]]
   [transport.rhythm :refer [compute-mm-from-dur-info get-dur-beats get-dur-info-for-beats get-dur-info-for-mm-and-millis get-dur-millis next-note-dur note-dur-to-millis]]
   [transport.settings :refer :all]
   [transport.volume :refer [select-volume select-volume-for-next-note]]
   [transport.util :refer :all]
   )
  (:import
   transport.melodychar.MelodyChar
   )
  )

(def CONTINUITY-PROBS [1 2 2 3 3 7 9 10 12 17])
(def SMOOTHNESS-PROBS [3 3 4 4 5 5 6 6 8 10])
(def loud-player (atom nil))        ;; player-id of loud player interrupt
(def loud-player-time (atom nil))   ;; start time of loud player interrupt

(defn melody-loud-interrupt-event
  [& {:keys [player-id time]}]
  (if (nil? @loud-player)
    (do
      (reset! loud-player player-id)
      (reset! loud-player-time time)
      (println "loud-interrupt-event loud-player:" @loud-player)
      ))
  )

(defn init-melody
  "Set loud player vars to nil and register listener for MSG-LOUD-INTERUPT-EVENT"
  []
  (reset! loud-player nil)
  (reset! loud-player-time nil)
  (register-listener
   MSG-LOUD-INTERUPT-EVENT
   transport.melody/melody-loud-interrupt-event
   nil
   )
  true
  )

(defn reset-melody
  []
  (init-melody)
  )

(defn- select-melody-continuity
  "Returns a number from 0 to 10 to determine how continuous
   the melody will be.
   0 - discontinuous (all rests) -> 9 - continuous (few rests)"
  ([] (weighted-choice CONTINUITY-PROBS))
  ([player]
     (weighted-choice CONTINUITY-PROBS)
     )
  ([player cntrst-plyr cntrst-melody-char]
     (let [cntrst-continuity (get-melody-char-continuity cntrst-melody-char)
           cntrst-continuity-probs (cond
                                    (and (> cntrst-continuity 0) (< cntrst-continuity 9))
                                    (assoc CONTINUITY-PROBS
                                      (dec cntrst-continuity ) 0
                                      cntrst-continuity 0
                                      (inc cntrst-continuity) 0)
                                    (= cntrst-continuity 0)
                                    (assoc CONTINUITY-PROBS 0 0 1 0)
                                    :else
                                    (assoc CONTINUITY-PROBS 8 0 9 0)
                                    )
           ]
       (weighted-choice cntrst-continuity-probs)
       ))
  )

(defn- select-melody-density
  "Returns a number from 0 to 9 to determine how dense
   the melody will be.
   0 - sparse  (few notes of long duration) -> 9 - dense (many notes of short duration"
  ([] (rand-int 10))
  ([player]
     (cond
      (= (get-behavior-action-for-player player) SIMILAR-ENSEMBLE)
      (get-ensemble-density)
      (= (get-behavior-action-for-player player) CONTRAST-ENSEMBLE)
      (let [ens-density (int (+ (get-average-density) 0.5))]
        (if (> ens-density 4) (- ens-density 5) (+ ens-density 5)))
      :else (rand-int 10))
     )
  ([player cntrst-plyr cntrst-melody-char]
     (let [cntrst-density (get-melody-char-density cntrst-melody-char)]
       (cond
        (and (> cntrst-density 0) (< cntrst-density 9))
        (let [density (rand-int 7)]
          (if (> density (dec cntrst-density)) density (+ density 3)))
        (= cntrst-density 0)
        (+ (rand-int 8) 2)
        :else
        (rand-int 8)
        )))
  )

(defn- select-melody-range
  "Returns a list that is the lo note and the hi note of
   the melody's range."
  ([lo-range hi-range]
     (let [range-lo (random-int lo-range hi-range)]
       (list range-lo (random-int range-lo hi-range))))
  ([player]
     (list (get-lo-range player) (get-hi-range player))
     )
  ([player cntrst-plyr cntrst-melody-char]
     ;; if CONTRASTing player has narrow range
     ;;   use as wide a range as possible
     ;;   else use a range no wider than an octave
     (let [cntrst-range (get-melody-char-range cntrst-melody-char)
           cntrst-hi (second cntrst-range)
           cntrst-lo (first cntrst-range)
           ]
       (if (< (- cntrst-hi cntrst-lo) 13)
         (do
           (list (get-lo-range player) (get-hi-range player)))
         (let [player-lo (get-lo-range player)
               player-hi (get-hi-range player)
               range-in-semitones (rand-int 13)
               melody-range-lo (cond
                                (or (> player-lo cntrst-hi) (< player-hi cntrst-lo))
                                player-lo
                                (<= (+  cntrst-hi range-in-semitones) player-hi)
                                (min (inc cntrst-hi) (second MIDI-RANGE))
                                (<= (+  player-lo range-in-semitones) cntrst-lo)
                                player-lo
                                (< player-lo cntrst-lo)
                                player-lo
                                :else
                                (max (- player-hi range-in-semitones) 0)
                                )
               ]
           (list melody-range-lo (min player-hi (+ melody-range-lo range-in-semitones))))
         )
       ))
  )

(defn- select-melody-smoothness
  "Returns a number from 0 to 9 to determine how smooth (stepwise)
   the melody will be (steps vs skips and changes in volume levels.
   0 - mostly steps, same voolume -> 9 - mostly skips (wide skips), large volume changes"
  ([] (weighted-choice SMOOTHNESS-PROBS))
  ([player]
     (weighted-choice SMOOTHNESS-PROBS)
     )
  ([player cntrst-plyr cntrst-melody-char]
     (let [cntrst-smoothness (get-melody-char-smoothness cntrst-melody-char)]
       (cond
        (and (> cntrst-smoothness 0) (< cntrst-smoothness 9))
        (let [smoothness (rand-int 7)]
          (if (> smoothness (dec cntrst-smoothness)) smoothness (+ smoothness 3)))
        (= cntrst-smoothness 0)
        (+ (rand-int 8) 2)
        :else
        (rand-int 8)
        )))
  )

(defn select-random-melody-characteristics
  [lo-range hi-range]
  (MelodyChar. (select-melody-continuity)
               (select-melody-density)
               (select-melody-range lo-range hi-range)
               (select-melody-smoothness))
  )

(defn select-melody-characteristics
  [player]
  (let [cntrst-plyr (if (= (get-behavior-action-for-player player) CONTRAST-PLAYER)
                      (get-player (get-behavior-player-id-for-player player))
                      nil)
        ]
    (if (= cntrst-plyr nil)
      (MelodyChar. (select-melody-continuity player)
                   (select-melody-density player)
                   (select-melody-range player)
                   (select-melody-smoothness player))
      (do (let [cntrst-melody-char (get-melody-char cntrst-plyr)]
            (MelodyChar. (select-melody-continuity player cntrst-plyr cntrst-melody-char)
                         (select-melody-density player cntrst-plyr cntrst-melody-char)
                         (select-melody-range player cntrst-plyr cntrst-melody-char)
                         (select-melody-smoothness player))
            )))
    )
  )

(defn- get-loud-event-prob
  [note-time]
  (let [time-diff (- note-time @loud-player-time)
        rest-prob (- 1 (* time-diff 0.00001))
        ]
    (if (<= rest-prob 0)
      (do
        (println "LOUD EVENT DONE")
        (reset! loud-player nil)
        (reset! loud-player-time nil)
        0
        )
      rest-prob
      )
    )
  )

(defn- loud-rest?
  "Should player rest because of loud interrupt
   Returns true for rest false to not rest

   player - player checking if it should rest
   note-time - the time (in millis) that the player is supposed to play"
  [player note-time]
  (let [loud-event-prob (if (or (nil? @loud-player) (= loud-player (get-player-id player)))
                           0
                           (get-loud-event-prob note-time))]
    (if (> loud-event-prob (rand)) true false)
   )
  )

(defn note-or-rest
  "Determines whether to play a note or rest.
   Returne true for note, false for rest

   player - the player to determine note or rest for"
  [player note-time]
  (if (loud-rest? player note-time)
    false     ;; rest because of loud interruption
    (let [play-note? (random-int 0 10)]
      (if (> (get-melody-char-continuity (get-melody-char player)) play-note?)
        true
        (if (not= 9 play-note?)                                ;; if play-note? not 9
          false                                                ;; rest
          (if (and                                             ;; else
               (not= {} (get-melody player))                   ;; if melody not empty
               (= 0                                            ;; and last pitch is root
                  (get-scale-degree
                   player
                   (or (get-last-melody-note player) 0)))      ;; or rest
               (< (rand) 0.8))                                 ;; possibly rest
            false
            true)))
      )
    ))

(defn note-or-rest-follow-ensemble
  [player note-time]
  (if (< 0.5 (get-rest-probability)) nil true))

(defn note-or-rest-contrast-ensemble
  [player note-time]
  (if (< 0.5 (get-rest-probability)) true nil))

(defn get-melody-event
  [player-id melody-event-no]
  (get (get-melody (get-player player-id)) melody-event-no))

(defn- compute-sync-time
  "Returns the time of a downbeat. The time returned completes any fractional
   part of the current beat and adds 1.  So, if the current beat is 1.5, this
   function will return the time for beat 3

   mm - the mm for beat
   beat - the beat to be measured
   time - the time beat occured beat"
  [mm beat time]
  (println "melody.cly - compute-sync-time mm:" mm "beat:" beat "time:" time)
  (let [fractional-beat (if (not= (int beat) beat)
                          (- 1 (- beat (int beat)))
                          0)]
    (+ time (note-dur-to-millis mm (+ 1 fractional-beat)))
    )
  )

(defn- sync-beat-follow
  [player follow-player event-time]
  (println)
  (print-msg "sync-beat-follow" "player: " (get-player-id player) " follow-player: " (get-player-id follow-player))
  (println)
  (let [follow-player-mm (get-mm follow-player)
        follow-player-beat (get-cur-note-beat follow-player)
        follow-player-time (get-cur-note-time follow-player)
        follow-player-last-melody-event (get-last-melody-event follow-player)
        last-seg-num (get-seg-num-for-event follow-player-last-melody-event)
        new-dur-info (if (or (= follow-player-beat nil) (= follow-player-beat 0))
                       ;; current info for FOLLOW player is for next segment
                       ;;  which means FOLLOW player is either syncing (nil) or resting before starting segment
                       ;;  so, sync time = cur-note-beat time + 1 beat
                       (do
                         (println "*********")
                         (println "melody.clj - sync-beat-follow get-dur-info-for-mm-and-millis 1 - new seg")
                         (println "*********")
                         (get-dur-info-for-mm-and-millis
                          follow-player-mm
                          (+ (- follow-player-time event-time) (note-dur-to-millis follow-player-mm 1)))
                         )
                       (do
                         (println "melody.clj - sync-beat-follow get-dur-info-for-mm-and-millis 2")
                         (get-dur-info-for-mm-and-millis
                          (get-mm player)
                          (- (compute-sync-time follow-player-mm follow-player-beat follow-player-time) event-time))
                         )
                       )
        ]
    (create-melody-event
     :note nil
     :dur-info new-dur-info
     :follow-note nil
     :instrument-info (get-instrument-info player)
     :volume 0
     :seg-num (get-seg-num player)
     )
    )
  )

(defn next-melody-follow
  [player event-time]
  (let [follow-player-id (get-behavior-player-id-for-player player)
        follow-player (get-player follow-player-id)
        follow-player-last-event (get-last-melody-event-num follow-player-id)
        last-follow-event-num (get-follow-note-for-event (get-last-melody-event player))
        player-seg-num (get-seg-num player)
        ]
    (cond
     (not (nil? (get-sync-beat-player-id player))) (sync-beat-follow player follow-player event-time)
     (or (nil? last-follow-event-num)
         (not= player-seg-num (get-seg-num-for-event (get-last-melody-event player)))
         )
      ;; first time or new segment, rest 3 beats
      (do
        (print-msg "next-melody-follow" "player: " (get-player-id player) " seg-num: " (get-seg-num player) " following seg-num: " (get-seg-num-for-event (get-last-melody-event player)) " follow-player-id: " follow-player-id)
        (create-melody-event
         :note nil
         :dur-info (get-dur-info-for-beats follow-player 3)
         :follow-note (if (nil? follow-player-last-event)
                        0
                        (- follow-player-last-event 1))
         ;; if follow-player has not played a note yet, get follow-player instrument-info
         ;; otherwise get instrument-info from last follow-player-event
         :instrument-info (if (nil? follow-player-last-event)
                            (get-instrument-info (get-player follow-player-id))
                            (get-instrument-info-for-event
                             (get-melody-event follow-player-id follow-player-last-event)))
         :volume 0  ;; 0 volume for rest
         :seg-num player-seg-num
         ))
      :else
      ;; play FOLLOWer melody event after last-melody event
      (let [
            event-num-to-play (inc last-follow-event-num)
            next-melody-event (get-melody-event follow-player-id event-num-to-play)
            ]
        (if (nil? next-melody-event)
          ;; unless
          ;; FOLLOWer ahead of FOLLOWed
          ;; then repeat whatever melody-event just played
          (do
            (println "*************** FOLLOWER AHEAD OF FOLLOWED ***************")
            (print-player player)
            (print-player-num follow-player-id)
            (println "*************** END FOLLOWER AHEAD OF FOLLOWED END ***************")
            (println "*************** END FOLLOWER AHEAD OF FOLLOWED END ***************")
            (println "*************** END FOLLOWER AHEAD OF FOLLOWED END ***************")
            (println "*************** END FOLLOWER AHEAD OF FOLLOWED END ***************")
            (println "*************** END FOLLOWER AHEAD OF FOLLOWED END ***************")
            (println "*************** END FOLLOWER AHEAD OF FOLLOWED END ***************")
            (println "*************** END FOLLOWER AHEAD OF FOLLOWED END ***************")
            (assoc (get-last-melody-event player) :seg-num player-seg-num)
            )
          (assoc next-melody-event :follow-note event-num-to-play :seg-num player-seg-num))
        )))
  )

(defn- next-melody-for-player
  [player event-time]

  (let [follow-player-id (get-sync-beat-player-id player)]
    (if follow-player-id
      (sync-beat-follow player (get-player follow-player-id) event-time)
      (let [next-note-or-rest (if (loud-rest? player event-time)
                                nil
                                (if (note-or-rest player event-time) (next-pitch player) nil))
            ]
        (create-melody-event
         :note next-note-or-rest
         :dur-info (next-note-dur player)
         :follow-note nil
         :instrument-info (get-instrument-info player)
         :volume (select-volume-for-next-note player event-time next-note-or-rest)
         :seg-num (get-seg-num player)
         ))))
  )

(defn next-melody
  "Returns the next note information as a map for player

    player - the player map"
  [player event-time]
  (if (nil? player) (println "melody.clj - next-melody - PLAYER IS NIL!!!!!!!!"))
  (cond
   (= (get-behavior-action-for-player player) FOLLOW-PLAYER) (next-melody-follow player event-time)
   ;; else pick next melody note based only on players settings
   ;;  do not reference other players or ensemble
   :else (next-melody-for-player player event-time))
  )
