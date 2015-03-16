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

(ns transport.play-note
  (:require
   [overtone.live :refer [apply-at ctl midi->hz node-live?]]
   [transport.behavior :refer [get-behavior-action get-behavior-player-id]]
   [transport.dur-info :refer [get-dur-beats get-dur-millis]]
   [transport.instrument :refer [has-release? play-instrument get-instrument-range-hi get-instrument-range-lo]]
   [transport.instrumentinfo :refer :all]
   [transport.melody :refer [next-melody]]
   [transport.melodyevent :refer :all]
   [transport.messages :refer :all]
   [transport.message-processor :refer [send-message register-listener unregister-listener]]
   [transport.players :refer :all]
   [transport.schedule :refer [sched-event]]
   [transport.sc-instrument :refer [stop-instrument]]
   [transport.segment :refer [first-segment new-segment new-segment? get-contrasting-info-for-player]]
   [transport.settings :refer :all]
   [transport.util.utils :refer :all]
   )
  (:import transport.behavior.Behavior)
  )

(defn new-contrast-info
  [& {:keys [change-player-id contrast-player-id originator-player-id melody-no]}]
  (new-contrast-info-for-player
   :change-player-id change-player-id
   :contrast-player-id contrast-player-id
   :originator-player-id originator-player-id
   :contrasting-info (get-contrasting-info-for-player (get-player-map contrast-player-id))
   )
  )

(defn- listeners-msg-new-segment
  "Called first time player plays a note in a new segment.
   Sends a message that it is in a new segment, and
   if the player is FOLLOWing, SIMILARing, or CONTRASTing
   it will listen for new segmemnts in the player it is
   FOLLOWing etc...

   player - the player starting a new segment
   melody-no - the key of the melody event that is being played"
  [player melody-no]
  (let [player-id (get-player-id player)]
       (send-message MSG-PLAYER-NEW-SEGMENT :change-player-id player-id :originator-player-id player-id)
       (send-message MSG-PLAYER-NEW-FOLLOW-INFO :change-player-id player-id :originator-player-id player-id :melody-no melody-no)
       (send-message MSG-PLAYER-NEW-SIMILAR-INFO :change-player-id player-id :originator-player-id player-id)
       (send-message MSG-PLAYER-NEW-CONTRAST-INFO :change-player-id player-id :originator-player-id player-id)

       (cond
        (= (get-behavior-action (get-behavior player)) SIMILAR-PLAYER)
        (register-listener
         MSG-PLAYER-NEW-SIMILAR-INFO
         transport.players/player-copy-new-similar-info
         {:change-player-id (get-behavior-player-id (get-behavior player))}
         :follow-player-id player-id
         )
        (= (get-behavior-action (get-behavior player)) CONTRAST-PLAYER)
        (register-listener
         MSG-PLAYER-NEW-CONTRAST-INFO
         transport.play-note/new-contrast-info
         {:change-player-id (get-behavior-player-id (get-behavior player))}
         :contrast-player-id player-id
         )
        )
       )
   )

(defn- update-player-with-new-segment
  "Get a new segment for player and unregister any listeners
   based on the previous segment. Returns player with new segment.

   player - the player to get a new segment for"
  [player event-time]
  (let [prev-behavior (get-behavior player) ;; behavior before new segment
        ]
    (cond
     (= (get-behavior-action prev-behavior) SIMILAR-PLAYER)
     (unregister-listener
      MSG-PLAYER-NEW-SIMILAR-INFO
      transport.players/player-copy-new-similar-info
      {:change-player-id (get-behavior-player-id prev-behavior)}
      :follow-player-id (get-player-id player)
      )
     (= (get-behavior-action prev-behavior) CONTRAST-PLAYER)
     (unregister-listener
      MSG-PLAYER-NEW-CONTRAST-INFO
      transport.play-note/new-contrast-info
      {:change-player-id (get-behavior-player-id prev-behavior)}
      :contrast-player-id (get-player-id player)
      )
     )
    )
  (new-segment player event-time)
  )

(defn- update-melody-list
  [cur-melody next-melody-no new-melody-event]
  (if (= (count cur-melody) SAVED-MELODY-LEN)
    (do
      (assoc (dissoc cur-melody (- next-melody-no SAVED-MELODY-LEN))
        next-melody-no  new-melody-event)
      )
    (assoc cur-melody next-melody-no new-melody-event)
    )
  )

(defn- update-melody-info
  [cur-melody player event-time melody-event new-seg? sync-beat-player-id]
  (let [prev-note-beat (get-cur-note-beat player)
        cur-note-beat (cond (not (nil? sync-beat-player-id)) nil
                            (nil? (get-cur-note-beat player)) 0 ;; right after sync beat this will be nill so reset it
                            (not (nil? (get-dur-info-for-event melody-event))) (+ (get-cur-note-beat player) (get-dur-beats (get-dur-info-for-event melody-event)))
                            :else 0)
        prev-note-time event-time
        cur-note-time (+ prev-note-time (get-dur-millis (get-dur-info-for-event melody-event)))
        next-melody-no (if (nil? (get-last-melody-event-num-for-player player))
                         1
                         (inc (get-last-melody-event-num-for-player player)))
        ]
    {
     :cur-note-beat (if new-seg? 0 cur-note-beat)
     :cur-note-time cur-note-time
     :melody (update-melody-list (:melody cur-melody) next-melody-no melody-event)
     :prev-note-beat prev-note-beat
     :prev-note-time prev-note-time
     }
    )
  )

(defn check-note-out-of-range
  [player-id melody-event]
  ;; Throw exception if note is out of instrument's range
  (if (or (> (get-note-for-event melody-event)
             (get-range-hi-for-inst-info (get-instrument-info-for-event melody-event)))
          (< (get-note-for-event melody-event)
             (get-range-lo-for-inst-info (get-instrument-info-for-event melody-event))))
    (do
      (print-msg "check-note-out-of-range" "ERROR ERROR ERROR  NOTE OUT OF INSTRUMENT RANGE!!!!  ERROR ERROR ERROR")
      (print-msg "check-note-out-of-range" "melody-event: " melody-event)
      (print-player-num player-id)
      (throw (Throwable. "NOTE OUT OF RANGE"))
      )
    )
  )

(defn stop-melody-note
  [melody-event player-id]
  "If player was not resting on last note, stops the note and returns true
   else returns false"
  (let [sc-instrument-id (get-sc-instrument-id melody-event)]
    (if sc-instrument-id
      (do
        (stop-instrument sc-instrument-id)
        )
      )
    )
  )

(defn get-actual-release-dur-millis
  [inst-info dur-millis]
  (let [release-dur (get-release-millis-for-inst-info inst-info)]
    (if (> dur-millis release-dur)
      release-dur
      0))
  )

(defn- articulate-note?
  [melody-event event-time]
  (let [dur-millis (get-dur-millis (get-dur-info-for-event melody-event))
        release-dur (get-actual-release-dur-millis
                     (get-instrument-info-for-event melody-event)
                     dur-millis
                     )
        ]
    (if (and (> release-dur 0)
             (> (- dur-millis (- (get-note-play-time-for-event melody-event) event-time ) release-dur)
                0)
             )
      true
      false)
    )
  )

(defn- check-live-synth
  [player]
  (let [last-melody-event-num (get-last-melody-event-num-for-player player)
        prior-melody-event (get-melody-event-num player (- last-melody-event-num 30))]
    (if (and (> (get-last-melody-event-num-for-player player) 30)
             (node-live? (get-sc-instrument-id prior-melody-event))
             (not= (get-sc-instrument-id prior-melody-event)
                   (get-sc-instrument-id (get-melody-event-num player (- last-melody-event-num 5))))
             (not= (get-sc-instrument-id prior-melody-event)
                   (get-sc-instrument-id (get-melody-event-num player (- last-melody-event-num 4))))
             (not= (get-sc-instrument-id prior-melody-event)
                   (get-sc-instrument-id (get-melody-event-num player (- last-melody-event-num 2))))
             (not= (get-sc-instrument-id prior-melody-event)
                   (get-sc-instrument-id (get-melody-event-num player (- last-melody-event-num 1))))
             (not= (get-sc-instrument-id prior-melody-event)
                   (get-sc-instrument-id (get-melody-event-num player last-melody-event-num)))
             )
      (do
        (binding [*out* *err*]
          (print-msg "check-live-synth" "SYNTH LIVE SYNTH LIVE SYNTH LIVE SYNTH LIVE SYNTH LIVE " )
          (print-msg "check-live-synth" "last-melody-event-num: " last-melody-event-num)
          (print-player player)
          )
        (throw (Throwable. "SYNTH LIVE"))
        )
      )
    )
  )


(defn- new-segment-for-following-player?
  "Returns true if the event after melody-event
   is a new segment in the following player.
   Compares the seg-num in the molody-event of the FOLLOWing player for the melody
   event passed in (or FOLLOWer's last melody-event) with the seg-num of the
   FOLLOWing player's next melody event. If they do not match, it is a new-seg in
   the FOLLOWing player.

   player - map for the player to check
   melody event - melody event to check or player's last melody event if omitted"
  [player & {:keys [melody-event]
             :or {melody-event (get-last-melody-event player)}}]

  (if (and (= (get-behavior-action (get-behavior player)) FOLLOW-PLAYER)
           (not= (get-follow-note-for-event melody-event) 0))
    (let [following-player (get-player-map (get-behavior-player-id (get-behavior player)))
          cur-following-event-seg (get-seg-num-for-event (get-melody-event-num following-player
                                                                               (get-follow-note-for-event melody-event)))
          next-following-event-seg (get-seg-num-for-event (get-melody-event-num following-player
                                                                                (inc (get-follow-note-for-event melody-event))))
          ]
      (if (not= cur-following-event-seg next-following-event-seg)
        true
        false)
      )
    false
    )
  )

(defn- check-note-off
  "Schedule a note off for the last note played
    by player if necessary and/or possible

   "
  [melody-event event-time]

  (let [inst-has-release? (has-release? (get-instrument-info melody-event))
        ;; all notes without release (AD or NE) are not articulated (stopped)
        articulate? (if inst-has-release?
                      (articulate-note? melody-event event-time)
                      false
                      )
        ]
    (if articulate?
          (do
            ;; (print-msg "check-note-off" "player-id: " (get-player-id-for-event melody-event) " note: " (get-note-for-event melody-event))
            (apply-at (+ event-time
                         (- (get-dur-millis (get-dur-info-for-event melody-event))
                            (get-release-millis-for-inst-info (get-instrument-info-for-event melody-event))
                            ))
                      stop-melody-note
                      [melody-event (get-player-id-for-event melody-event)]))
          )
  ))

(defn play-melody
  "

   player - map for the current player
   player-id - the id of the current player
   event-time - time this note event was scheduled for"
  [player-id player event-time new-seg? new-seg-following-player? sync-beat-player-id]

;;  (println)
;;  (print-msg "play-melody"  "player-id: " player-id " event-time: " event-time " current time: " (System/currentTimeMillis))
  (let [last-melody-event-num (get-last-melody-event-num-for-player player)
        last-melody-event (get-melody-event-num player last-melody-event-num)
        last-melody-event-note (get-note-for-event last-melody-event)
        inst-has-release? (if (nil? last-melody-event-note)
                            false
                            (has-release? (get-instrument-info last-melody-event)))
        ;; all notes without release and notes following rests are articulated (articulate true?)
        articulate? (cond
                     (not last-melody-event-note) true
                     inst-has-release? (articulate-note? last-melody-event (get-prev-note-time player))
                     :else true
                     )
        ;; will the new melody event start a new segment?
        melody-event (next-melody player event-time sync-beat-player-id)
        melody-event-note (get-note-for-event melody-event)
        sc-instrument-id (if (not (nil? melody-event-note))
                           (cond
                            (or articulate?
                                new-seg?
                                new-seg-following-player?
                                )
                            ((get-instrument-for-inst-info (get-instrument-info-for-event melody-event))
                             (midi->hz (get-note-for-event melody-event))
                             (get-volume-for-event melody-event)
                             )
                            :else
                            (let [inst-id (get-sc-instrument-id last-melody-event)]
                              (ctl inst-id :freq (midi->hz (get-note-for-event melody-event)))
                              inst-id
                              )
                            )
                           nil
                           )
        note-play-time (max (System/currentTimeMillis) event-time)
        upd-melody-event (set-sc-instrument-id-and-note-play-time  melody-event
                                                                   sc-instrument-id
                                                                   note-play-time)
        new-melody (reset! (get @player-melodies player-id)
                           (update-melody-info (deref (get @player-melodies player-id))
                                               player
                                               event-time
                                               upd-melody-event
                                               new-seg?
                                               sync-beat-player-id
                                               )
                           )
        ]

    (if (not (nil? melody-event-note))
      ;; if about to play a note, check range
      (do
        (check-note-out-of-range player-id upd-melody-event)
        (if (and (or new-seg? new-seg-following-player?)
                 (not articulate?)
                 inst-has-release?
                 )
          (stop-melody-note last-melody-event player-id)
          )
        )
      ;; if about to rest, make sure prior note is off
      (if (and (not articulate?)
               inst-has-release?
               )
        (stop-melody-note last-melody-event player-id)
        )
      )

    (if (nil? (get-dur-millis (get-dur-info-for-event melody-event)))
      (print-msg "play-melody" "MELODY EVENT :DUR IS NILL !!!!"))

    upd-melody-event
    ))

(defn next-note
  [player-id event-time]
  (let [player (get-player-map player-id)
        last-melody-event-num (get-last-melody-event-num-for-player player)
        last-melody-event (get-melody-event-num player last-melody-event-num)
        ;; will the new melody event start a new segment?
        new-seg? (>= event-time (+ (get-seg-start player) (get-seg-len player)))
        new-seg-following-player? (new-segment-for-following-player? player
                                                                     :melody-event last-melody-event)
        new-player (cond new-seg?
                         (swap! (get @ensemble player-id) update-player-with-new-segment event-time)
                         new-seg-following-player?
                         (swap! (get @ensemble player-id)
                                update-player-follow-info
                                (get-player-map (get-behavior-player-id
                                                 (get-behavior player)))
                                (inc last-melody-event-num)
                                )
                             :else
                             player
                             )
        sync-beat-player-id (cond (and new-seg?
                                       (= (get-behavior-action (get-behavior new-player)) FOLLOW-PLAYER)
                                       )
                                  (get-behavior-player-id (get-behavior new-player))
                                  (and new-seg?
                                       (= (get-behavior-action (get-behavior new-player)) SIMILAR-ENSEMBLE)
                                       )
                                  (get-player-with-mm player (get-mm new-player))
                                  :else
                                  nil
                                  )
        melody-info (play-melody player-id
                                 new-player
                                 event-time
                                 new-seg?
                                 new-seg-following-player?
                                 sync-beat-player-id
                                 )
        melody-event (get-last-melody-event new-player)
        melody-event-note (get-note-for-event melody-event)
        melody-dur-millis (get-dur-millis (get-dur-info-for-event melody-event))
        ]
    (if melody-event-note
      (check-note-off melody-event event-time)
      )
    ;; if this is the start of a new segment, send messages
    (if (= (get-seg-start new-player) event-time)
      (listeners-msg-new-segment new-player (get-last-melody-event-num-for-player new-player))
      )
    (send-message MSG-PLAYER-NEW-NOTE :player new-player :note-time event-time)

    (check-live-synth new-player)

    (sched-event 0
                 (get-player-val new-player "function") player-id
                 :time (+ event-time melody-dur-millis))
    )
  )

(defn play-first-melody-note
  "Gets the first note to play and plays it (if it is not a rest)

   This function is only used for the very first note an instrument plays.
   For notes after the first, this function sets the player to use play-melody.

   player - map for the current player
   player-id - the id of the current player
   event-time - time this note event was scheduled for"
  [player player-id event-time]

  (let [melody-event (next-melody player event-time nil)
        sc-instrument-id (if (not (nil? (:note melody-event)))
                             ((get-instrument-for-inst-info (get-instrument-info-for-event melody-event))
                              (midi->hz (get-note-for-event melody-event))
                              (get-volume-for-event melody-event)
                              )
                             nil
                             )
        note-play-time (max (System/currentTimeMillis) event-time)
        upd-melody-event (set-sc-instrument-id-and-note-play-time melody-event
                                                                  sc-instrument-id
                                                                  note-play-time)
        new-melody (reset! (get @player-melodies player-id)
                           (update-melody-info (deref (get @player-melodies player-id))
                                              player
                                              event-time
                                              upd-melody-event
                                              false ;; not new-seg?
                                              nil   ;; no sync-beat-player-id
                                              )
                          )
        ]

    (if (not (nil? (get-note-for-event upd-melody-event)))
      ;; if about to play a note, check range
      (check-note-out-of-range player-id upd-melody-event))

    upd-melody-event
    ))

(defn first-note
  [player-id event-time]

  (let [new-player (swap! (get @ensemble player-id) set-first-note event-time transport.play-note/next-note)
        melody-event (play-first-melody-note new-player player-id event-time)
        melody-dur-millis (get-dur-millis (get-dur-info-for-event melody-event))
        release-time (-
                      (+ event-time melody-dur-millis)
                      (if (nil? (:note melody-event))
                        0
                        (get-actual-release-dur-millis (get-instrument-info new-player) melody-dur-millis)) )
        ]

    (send-message MSG-PLAYER-NEW-NOTE :player new-player :note-time event-time)
    (if (get-note-for-event melody-event)
      (check-note-off melody-event event-time))

    (sched-event 0
                 (get-player-val new-player "function") player-id
                 :time release-time)
    )
  )

(defn reset-ensemble
  []
  (swap! ensemble clear-ensemble)
  (swap! player-melodies clear-player-melodies)
  )

(defn create-player
  [player-no]
  (first-segment {:function transport.play-note/first-note
                  :player-id player-no
                  })
  )

(defn create-init-melody
  [player-no]
  {:cur-note-beat 0
   :cur-note-time 0
   :melody {}
   :prev-note-beat 0
   :prev-note-time 0
   }
  )

(defn init-ensemble
  []
  (let [all-players (map create-player (range @number-of-players))
        ;; set the :behavior :player-id for all players that are FOLLOWing, SIMILARing or CONTRASTing other players
        final-players (zipmap
                       (map get all-players (repeat :player-id))
                       (map atom
                            (map assoc
                                 all-players
                                 (repeat :behavior)
                                 (map select-and-set-behavior-player-id
                                      all-players
                                      (repeat :all-players)
                                      (repeat all-players))
                                 )))
        init-melodies (zipmap
                       (map get all-players (repeat :player-id))
                       (map atom
                            (map create-init-melody
                                 (map get all-players (repeat :player-id)))
                            ))
        ]
    (reset-ensemble)
    (reset! ensemble final-players)
    (reset! player-melodies init-melodies)
    )

  (print-all-players)

  ;; Schedule first event for all players
  (dorun (map sched-event
              (repeat 0)
              (map get-player-val (get-ensemble) (repeat "function"))
              (map get-player-id (get-ensemble))))
  )
