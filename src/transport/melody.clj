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
  (::require
   [overtone.live :refer [MIDI-RANGE]]
   [transport.behaviors :refer [get-behavior-action-for-player get-behavior-ensemble-action-for-player get-behavior-player-id-for-player]]
   [transport.pitch :refer [get-scale-degree next-pitch]]
   [transport.ensemble-status :refer [get-average-volume get-rest-probability]]
   [transport.instrument :refer [get-hi-range get-lo-range]]
   [transport.melodychar :refer [get-melody-char-continuity get-melody-char-density get-melody-char-range get-melody-char-smoothness]]
   [transport.messages :refer :all]
   [transport.message-processor :refer [register-listener]]
   [transport.players :refer :all]
   [transport.random :refer [random-int weighted-choice]]
   [transport.rhythm :refer [get-dur-info-for-beats next-note-dur]]
   [transport.settings :refer :all]
   [transport.volume :refer [select-volume select-volume-in-range]]
   )
  (:import transport.melodychar.MelodyChar)
  )

(def CONTINUITY-PROBS [4 3 2 1 1 1 1 2 2 1])
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
   0 - continuous (few rests) -> 9 - discontinuous (all rests)"
  ([] (weighted-choice CONTINUITY-PROBS))
  ([player]
     (weighted-choice CONTINUITY-PROBS)
     )
  ([player cntrst-plyr cntrst-melody-char]
     (let [cntrst-continuity (get-melody-char-continuity cntrst-melody-char)
           cntrst-continuity-probs (cond
                                    (and (> cntrst-continuity 0) (< cntrst-continuity 9))
                                    (assoc CONTINUITY-PROBS
                                      (- cntrst-continuity 1) 0
                                      cntrst-continuity 0
                                      (+ cntrst-continuity 1) 0)
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
     (rand-int 10)
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
  "Returns a number that is the maximum width of
   the melody's range in semitones."
  ([lo-range hi-range]
     (let [range-lo (random-int lo-range hi-range)]
       (list range-lo (random-int range-lo hi-range))))
  ([player]
     (list (get-lo-range player) (get-hi-range player))
     )
  ([player cntrst-plyr cntrst-melody-char]
     (let [cntrst-range (get-melody-char-range cntrst-melody-char)]
       ;; if CONTRASTing player has narrow range
       ;;   use as wide a range as possible
       ;;   else use a range no wider than an octave
       (let [cntrst-hi (second cntrst-range)
             cntrst-lo (first cntrst-range)]
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
                                  (inc cntrst-hi)
                                  (<= (+  player-lo range-in-semitones) cntrst-lo)
                                  player-lo
                                  (< player-lo cntrst-lo)
                                  player-lo
                                  :else
                                  (- player-hi range-in-semitones)
                                  )
                 ]
             (list melody-range-lo (+ melody-range-lo range-in-semitones)))
           ))
       ))
  )

(defn- select-melody-smoothness
  "Returns a number from 0 to 9 to determine how smooth (stepwise)
   the melody will be.
   0 - mostly steps -> 9 - mostly skips (wide skips)"
  ([] (rand-int 10))
  ([player]
     (rand-int 10)
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
  (let [cntrst-plyr (if (= (get-behavior-action-for-player player) CONTRAST)
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
  (let [loud-event-prob (if (or (nil? @loud-player) (= get-player-id player))
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
      (if (< (get-melody-char-continuity (get-melody-char player)) play-note?)
        true
        (if (not= 0 play-note?)                                ;; if play-note? not 0
          false                                                ;; rest
          (if (and                                             ;; else
               (not= {} (get-melody player))                   ;; if melody not empty
               (= 0                                            ;; and last pitch is root
                  (get-scale-degree
                   player
                   (or (get-last-melody-note player) 0)))      ;; or rest
               (< (rand) 0.8))                                 ;; possibly rest
            false
            true))))))

(defn note-or-rest-follow-ensemble
  [player note-time]
  (if (< 0.5 (get-rest-probability)) nil true))

(defn note-or-rest-contrast-ensemble
  [player note-time]
  (if (< 0.5 (get-rest-probability)) true nil))

(defn get-volume
  [melody-event]
  (:volume melody-event))

(defn get-melody-event
  [player-id melody-event-no]
  (get (get-melody (get-player player-id)) melody-event-no))

(defn next-melody-follow
  [player]
  (let [follow-player-id (get-behavior-player-id-for-player player)
        follow-player-last-note (get-last-melody-event-num follow-player-id)
        next-new-event {:note nil
                        :dur-info (get-dur-info-for-beats (get-player follow-player-id) 3)
                        :follow-note (if (nil? follow-player-last-note)
                                       0
                                       (- follow-player-last-note 1))
                        :volume (select-volume player)
                        }
        ]
    (if (nil? (:follow-note (get-last-melody-event player)))
      ;; first time, rest 3 beats
      next-new-event
      ;; else
      ;; play FOLLOWer melody event after last-melody event
      (let [
            last-melody-event-played (get-last-melody-event player)
            cur-note-to-play (+ (:follow-note last-melody-event-played) 1)
            next-melody-event (get-melody-event follow-player-id cur-note-to-play)
            ]
        (if (nil? next-melody-event)
          ;; unless
          ;; FOLLOWer ahead of FOLLOWed
          ;; then repeat whatever melody-event just played
          (assoc last-melody-event-played :follow-note (:follow-note last-melody-event-played))
          (assoc next-melody-event :follow-note cur-note-to-play))
        )))
  )

(defn- next-melody-complement-ensemble
  [player event-time]
  (let [next-note-or-rest (if (loud-rest? player event-time)
                            nil
                            (if (note-or-rest-follow-ensemble player event-time) (next-pitch player) nil)
                            )
        average-volume (get-average-volume)
        ]
;;    (println "average-volume: " average-volume)
    {:note next-note-or-rest
     :dur-info (next-note-dur player)
     :volume (select-volume-in-range
              (if (< average-volume 0.1) 0 (- average-volume 0.1))  ;; set range of volume to
              (if (> average-volume 0.9) 1 (+ average-volume 0.1))) ;; + or - 0.1 of average volume
     }
    ))

(defn- next-melody-contrast-ensemble
  [player event-time]
  (let [next-note-or-rest (if (loud-rest? player event-time)
                            nil
                            (if (note-or-rest-contrast-ensemble player event-time) (next-pitch player) nil))
        average-volume (get-average-volume)
        ]
;;    (println "average-volume: " average-volume)
    {:note next-note-or-rest
     :dur-info (next-note-dur player)
     :volume (select-volume-in-range
              (if (< average-volume 0.5) 0.5 0)  ;; set range of volume eithe 0.5 - 1 or
              (if (< average-volume 0.5) 1 0.5)) ;; 0 - 0.5 opposite of ensemble
     }
    ))

(defn- next-melody-for-player
  [player event-time]
  (let [play-note? (note-or-rest player event-time)
        ]
       {:note (if play-note? (next-pitch player) nil)
        :dur-info (next-note-dur player)
        :volume (if play-note? (select-volume player) 0) ;; 0 volume if rest
        })
  )

(defn next-melody
  "Returns the next note information as a map for player

    player - the player map"
  [player event-time]
  (cond
   (= (get-behavior-action-for-player player) FOLLOW) (next-melody-follow player)
   (= (get-behavior-ensemble-action-for-player player) COMPLEMENT) (next-melody-complement-ensemble player event-time)
   (= (get-behavior-ensemble-action-for-player player) CONTRAST) (next-melody-contrast-ensemble player event-time)
   ;; else pick next melody note based only on players settings
   ;;  do not reference other players or ensemble
   :else (next-melody-for-player player event-time))

  )
