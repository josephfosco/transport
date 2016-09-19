;    Copyright (C) 2013-2016  Joseph Fosco. All Rights Reserved
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
   [polyphony.core :refer [reset-variable-vals set-var]]
   [transport.behavior :refer [get-behavior-action get-behavior-player-id]]
   [transport.constants :refer :all]
   [transport.curplayer :refer [create-curplayer get-original-player
                                get-updated-player new-follow-info? new-segment?
                                set-updated-player]]
   [transport.dur-info :refer [get-dur-beats get-dur-millis]]
   [transport.ensemble-status :refer [get-average-mm get-ensemble-density get-density-trend]]
   [transport.instrument :refer [has-release?]]
   [transport.instrumentinfo :refer :all]
   [transport.melody :refer [next-melody]]
   [transport.melodychar :refer [get-melody-char-density]]
   [transport.melody.melodyevent :refer [get-dur-info-for-event get-follow-note-for-event
                                         get-instrument-info-for-event
                                         get-note-for-event get-note-play-time-for-event
                                         get-player-id-for-event
                                         get-sc-instrument-id get-volume-for-event
                                         set-sc-instrument-id-and-note-play-time]]
   [transport.messages :refer :all]
   [transport.message-processor :refer [send-message register-listener unregister-listener]]
   [transport.players :refer :all]
   [transport.schedule :refer [sched-event]]
   [transport.sc-instrument :refer [stop-instrument]]
   [transport.segment :refer [first-segment new-segment get-contrasting-info-for-player]]
   [transport.settings :refer [get-setting] :as setting]
   [transport.util.log :as log]
   [transport.util.print :refer [print-msg]]
   [transport.util.util-constants :refer [DECREASING INCREASING]]
   [transport.util.utils :refer :all]
   )
  (:import transport.behavior.Behavior
           transport.curplayer.CurPlayer)
  )

(def volume-adjust (atom 1))
(def cur-player-info (atom nil))

(defn new-contrast-info
  [& {:keys [change-player-id contrast-player-id originator-player-id melody-no]}]
  (if (not= originator-player-id change-player-id)
    (new-contrast-info-for-player
     :change-player-id change-player-id
     :contrast-player-id contrast-player-id
     :originator-player-id originator-player-id
     :contrasting-info (get-contrasting-info-for-player (get-player-map change-player-id) (get-player-map contrast-player-id))
     ))
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
  (let [player-id (get-player-id player)
        behavior-action (get-behavior-action (get-behavior player))
        ]
       (cond
        (= behavior-action FOLLOW-PLAYER)
        (register-listener
         MSG-PLAYER-NEW-FOLLOW-INFO
         transport.players/new-follow-info-for-player
         {:change-player-id (get-behavior-player-id (get-behavior player))}
         :follow-player-id player-id
         )
        (= behavior-action SIMILAR-PLAYER)
        (register-listener
         MSG-PLAYER-NEW-SIMILAR-INFO
         transport.players/player-copy-new-similar-info
         {:change-player-id (get-behavior-player-id (get-behavior player))}
         :follow-player-id player-id
         )
        (= behavior-action CONTRAST-PLAYER)
        (register-listener
         MSG-PLAYER-NEW-CONTRAST-INFO
         transport.play-note/new-contrast-info
         {:change-player-id (get-behavior-player-id (get-behavior player))}
         :contrast-player-id player-id
         )
        )

       (send-message MSG-PLAYER-NEW-SEGMENT :change-player-id player-id :originator-player-id player-id)
       (send-message MSG-PLAYER-NEW-FOLLOW-INFO
                     :change-player-id player-id
                     :originator-player-id player-id
                     :melody-no melody-no
                     :follow-info (get-following-info-from-player player))
       (send-message MSG-PLAYER-NEW-SIMILAR-INFO :change-player-id player-id :originator-player-id player-id)
       (send-message MSG-PLAYER-NEW-CONTRAST-INFO :change-player-id player-id :originator-player-id player-id)

       )
   )

(defn update-player-with-new-segment
  "Get a new segment for player and unregister any listeners
   based on the previous segment. Returns player with new segment.

   player - the player to get a new segment for"
  [player event-time]
  (let [behavior-action (get-behavior-action (get-behavior player))
        behavior-player-id (get-behavior-player-id (get-behavior player))
        ]
    (cond
     (= behavior-action FOLLOW-PLAYER)
     (unregister-listener
      MSG-PLAYER-NEW-FOLLOW-INFO
      transport.players/new-follow-info-for-player
      {:change-player-id behavior-player-id}
      :follow-player-id (get-player-id player)
      )
     (= behavior-action SIMILAR-PLAYER)
     (unregister-listener
      MSG-PLAYER-NEW-SIMILAR-INFO
      transport.players/player-copy-new-similar-info
      {:change-player-id behavior-player-id}
      :follow-player-id (get-player-id player)
      )
     (= behavior-action CONTRAST-PLAYER)
     (unregister-listener
      MSG-PLAYER-NEW-CONTRAST-INFO
      transport.play-note/new-contrast-info
      {:change-player-id behavior-player-id}
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
  [cur-melody player event-time melody-event sync-beat-player-id]
  (let [prev-note-beat (get-cur-note-beat player)
        cur-note-beat (cond (not (nil? sync-beat-player-id)) nil
                            (nil? (get-cur-note-beat player)) 0 ;; right after sync beat this will be nill so reset it
                            (new-segment? @cur-player-info) 0
                            (not (nil? (get-dur-info-for-event melody-event))) (+ (get-cur-note-beat player) (get-dur-beats (get-dur-info-for-event melody-event)))
                            :else 0)
        prev-note-time event-time
        cur-note-time (+ prev-note-time (get-dur-millis (get-dur-info-for-event melody-event)))
        next-melody-no (if (nil? (get-last-melody-event-num-for-player player))
                         1
                         (inc (get-last-melody-event-num-for-player player)))
        ]
    {
     :cur-note-beat cur-note-beat
     :cur-note-time cur-note-time
     :melody (update-melody-list (:melody cur-melody) next-melody-no melody-event)
     :player-id (get-player-id-for-melody cur-melody)
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

(defn- check-release-complete?
  [player last-melody-event-num prior-melody-event-num & {:keys [log-result?] :or {log-result? false}}]
  (let [prior-melody-event (get-melody-event-num player prior-melody-event-num)
        prior-instr (get-sc-instrument-id (get-melody-event-num player prior-melody-event-num))
        release-millis (get-release-millis-for-inst-info
                        (get-instrument-info-for-event prior-melody-event))
        new-instr-durs-millis (for [ndx (reverse (range prior-melody-event-num last-melody-event-num))
                                    :while (not= prior-instr (get-sc-instrument-id (get-melody-event-num player ndx)))
                                    ]
                                (get-dur-millis (get-dur-info-for-event (get-melody-event-num player ndx)))
                                )
        new-instr-total-dur (apply + new-instr-durs-millis)
        prior-instr-last-dur (get-dur-millis (get-dur-info-for-event
                                              (get-melody-event-num
                                               player
                                               (dec (- last-melody-event-num (count new-instr-durs-millis))))))
        ]
    (if log-result?
      (log/info (log/format-msg "check-release-complete?"
                                "release millis: " release-millis
                                " new-instr-total-dur: " new-instr-total-dur
                                " prior-instr-last-dur: " prior-instr-last-dur
                                " return: " (> new-instr-total-dur (+ release-millis prior-instr-last-dur 10)))))
    (> new-instr-total-dur (+ release-millis prior-instr-last-dur 20))
    )
  )

(defn- check-live-synth
  [player]
  (let [last-melody-event-num (get-last-melody-event-num-for-player player)
        prior-melody-event (get-melody-event-num player (- last-melody-event-num 30))]
    (if (and (> last-melody-event-num 30)
             (node-live? (get-sc-instrument-id prior-melody-event))
             (check-release-complete? player last-melody-event-num (- last-melody-event-num 30))
             )
      (do
        (log/error (with-out-str (transport.schedule/print-lateness)))
        (check-release-complete? player last-melody-event-num (- last-melody-event-num 30) :log-result? true)
        (binding [*out* *err*]
          (log/error (log/format-msg "check-live-synth" "SYNTH LIVE SYNTH LIVE SYNTH LIVE SYNTH LIVE SYNTH LIVE " ))
          (log/error (log/format-msg "check-live-synth" "last-melody-event-num: " last-melody-event-num))
          (log/error (with-out-str (print-player player)))
          )
        (throw (Throwable. "SYNTH LIVE"))
        )
      )
    )
  )

(defn- get-sync-beat-player-id
  [player-id player new-seg?]
  (cond (and new-seg?
             (= (get-behavior-action (get-behavior player)) FOLLOW-PLAYER)
             )
        (get-behavior-player-id (get-behavior player))
        (and new-seg?
             (= (get-behavior-action (get-behavior player)) SIMILAR-ENSEMBLE)
             )
        (if-let [mm-player (get-player-with-mm player (get-mm player))]
          ;; if there are no players with this player's mm
          ;; just return the player-id 1 higher than this id
          ;; (this is very very rare)
          mm-player
          (mod (inc player-id) @setting/number-of-players))
        :else
        nil
        )
  )

(defn- play-note-with-articulation
  [melody-event melody-event-note]
  ((get-instrument-for-inst-info (get-instrument-info-for-event melody-event))
   (midi->hz melody-event-note)
   (* (get-volume-for-event melody-event) @volume-adjust)
   )
  )

(defn- play-note-no-articulation
  [melody-event-for-instrument note-to-play]
  (let [inst-id (get-sc-instrument-id melody-event-for-instrument)]
    (ctl inst-id :freq (midi->hz note-to-play))
    inst-id
    )
  )

(defn- get-and-play-next-melody-event
  [player-id player event-time last-melody-event articulate?]
  (let [new-seg? (new-segment? @cur-player-info)
        sync-beat-player-id (get-sync-beat-player-id player-id player new-seg?)
        melody-event (next-melody player event-time sync-beat-player-id new-seg?)
        melody-event-note (get-note-for-event melody-event)
        ;; now play the note with the current instrument
        sc-instrument-id (if (nil? melody-event-note)
                           nil
                           (if (or articulate? new-seg? (new-follow-info? @cur-player-info))
                             (play-note-with-articulation melody-event melody-event-note)
                             (play-note-no-articulation last-melody-event melody-event-note)
                             )
                           )
        note-play-time (max (System/currentTimeMillis) event-time)
        upd-melody-event (set-sc-instrument-id-and-note-play-time melody-event
                                                                  sc-instrument-id
                                                                  note-play-time)
        ]

    (swap! (get @player-melodies player-id)
           update-melody-info
           player
           event-time
           upd-melody-event
           sync-beat-player-id
           )
    upd-melody-event
    )
  )

(defn- play-melody
  "Select and play the next melody note (or rest) for player.
   Stops prior note, if necessary.
   Returns melody-event for this note (or rest)
   player - map for the current player
   player-id - the id of the current player
   event-time - time this note event was scheduled for"
  [player-id player event-time]

  (let [last-melody-event-num (get-last-melody-event-num-for-player player)
        last-melody-event (get-melody-event-num player last-melody-event-num)
        last-melody-event-note (get-note-for-event last-melody-event)
        inst-has-release? (if (nil? last-melody-event-note)
                            false
                            (has-release? (get-instrument-info last-melody-event)))
        ;; all notes without release and notes following rests are articulated (articulate true?)
        articulate? (if (and last-melody-event-note inst-has-release?)
                      (articulate-note? last-melody-event (get-prev-note-time player))
                      true
                      )
        ;; now play the next note
        upd-melody-event (get-and-play-next-melody-event player-id
                                                         player
                                                         event-time
                                                         last-melody-event
                                                         articulate?
                                                         )
        ]

    ;; if the current note was not stopped and
    ;; the player is about to rest or the player's instrument is changing,
    ;; stop the current note
    (when (and (not articulate?) inst-has-release?)
      (when (or (nil? (get-note-for-event upd-melody-event))
                (or (new-segment? @cur-player-info)
                    (new-follow-info? @cur-player-info)
                    ))
        (stop-melody-note last-melody-event player-id))
      )

    (when (and (get-setting "validate-run")
               (not (nil? (get-note-for-event upd-melody-event))))
      (check-note-out-of-range player-id upd-melody-event))

    (if (nil? (get-dur-millis (get-dur-info-for-event upd-melody-event)))
      (log/info (log/format-msg "play-melody" "MELODY EVENT :DUR IS NILL !!!!")))

    upd-melody-event
    )
  )


(defn update-based-on-ensemble-density
  [player]
  (let [player-density (get-melody-char-density (get-melody-char player))]
    (cond (= (get-density-trend) INCREASING)
          (if (< player-density 9)
            (set-density player (inc player-density))
            player)
          (= (get-density-trend) DECREASING)
          (if (> player-density 0)
            (set-density player (dec player-density))
            player)
          :else
          player
          )
    )
  )

(defn update-based-on-ensemble
  [player]
  (-> (if (= (type (get-cur-note-beat player)) Long) ;; current note is on the beat
        (let [mm-diff (- (get-average-mm) (get-mm player))]
          (cond (> mm-diff @setting/ensemble-mm-change-threshold)
                (set-mm player (+ (get-mm player) 2))
                (< mm-diff (* @setting/ensemble-mm-change-threshold -1))
                (set-mm player (- (get-mm player) 2))
                :else
                player
                ))
        player
        )
      (update-based-on-ensemble-density)
      )
  )

(defn- check-note-off
  "Schedule a note off for the last note played
    by player if necessary and/or possible

   "
  [melody-event event-time]

  (let [;; all notes without release (AD or NE) are not articulated (stopped)
        articulate? (if (has-release? (get-instrument-info melody-event))
                      (articulate-note? melody-event event-time)
                      false
                      )
        ]
    (if articulate?
          (do
            (apply-at (+ event-time
                         (- (get-dur-millis (get-dur-info-for-event melody-event))
                            (get-release-millis-for-inst-info (get-instrument-info-for-event melody-event))
                            ))
                      stop-melody-note
                      [melody-event (get-player-id-for-event melody-event)]))
          )
  ))

(intern (ns-name 'polyphony.variables) '?player-updated (atom nil))
(intern (ns-name 'polyphony.variables) '?new-follow-info (atom nil))
(intern (ns-name 'polyphony.variables) '?needs-new-segment (atom nil))
(intern (ns-name 'polyphony.variables) '?similar-ensemble (atom nil))

(defn- update-player
  [player]
  ;; need to let these variables due to the wa the set-var macro is working
  (let [new-seg? (new-segment? @cur-player-info)
        upd-follow-info? (new-follow-info? @cur-player-info)
        ]
    (set-var ?needs-new-segment new-seg?)
    (set-var ?new-follow-info upd-follow-info?)
    (if (= (get-behavior-action (get-behavior player)) SIMILAR-ENSEMBLE)
      (set-var ?similar-ensemble true)
      )
    (reset! (get @ensemble (get-player-id player))
            (get-updated-player @cur-player-info)
            )
    )
  )

(defn- play-next-note
  [player event-time]
  (let [;; Select the next note and play it
        melody-event (play-melody (get-player-id player)
                                  player
                                  event-time
                                  )]

    (if (get-note-for-event melody-event)
      (check-note-off melody-event event-time)
      )
    )
  )

(defn send-new-note-msgs
  [orig-player new-player event-time]
  (let [player-id (get-player-id orig-player)]
    (cond (new-segment? @cur-player-info)
          (listeners-msg-new-segment new-player (get-last-melody-event-num-for-player new-player))
          (not= orig-player new-player)
          (send-msg-new-player-info player-id player-id (get-last-melody-event-num-for-player new-player) )
          )
    )

  (send-message MSG-PLAYER-NEW-NOTE :player new-player :note-time event-time)
  )

(defn next-note
  "Gets, plays and records the player's next note"
  [player-id event-time]

  (reset! cur-player-info (create-curplayer player-id event-time))
  (reset-variable-vals)
  (set-var ?player-updated false)

  (let [player (get-original-player @cur-player-info)
        new-player (update-player player)
        ]
    (play-next-note new-player event-time)
    (send-new-note-msgs player
                        new-player
                        event-time
                        )

    (if (get-setting "validate-run")
         (check-live-synth new-player))

    (sched-event 0
                 (get-player-val new-player "function") player-id
                 :time (+ event-time
                          (get-dur-millis (get-dur-info-for-event (get-last-melody-event new-player))))))
  )


(defn play-first-melody-note
  "Gets the first note to play and plays it (if it is not a rest)

   This function is only used for the very first note an instrument plays.
   For notes after the first, this function sets the player to use play-melody.

   player - map for the current player
   player-id - the id of the current player
   event-time - time this note event was scheduled for"
  [player player-id event-time]

  (log/data2 (log/format-msg
              "play-first-melody-note" "player-id: " player-id
              " event-time: " event-time
              " action: " (get-behavior-action (get-behavior player))))
  (let [melody-event (next-melody player
                                  event-time
                                  (if (= (get-behavior-action (get-behavior player)) FOLLOW-PLAYER)
                                    (get-behavior-player-id (get-behavior player))
                                    nil
                                  )
                                  true   ;; new segment
                                  )
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
        new-melody (swap! (get @player-melodies player-id)
                           update-melody-info
                           player
                           event-time
                           upd-melody-event
                           nil  ;; no sync-beat-player-id
                          )
        ]

    (if (not (nil? (get-note-for-event upd-melody-event)))
      ;; if about to play a note, check range
      (check-note-out-of-range player-id upd-melody-event))

    upd-melody-event
    ))

(defn first-note
  [player-id event-time]

  (log/data2 (log/format-msg "first-note" "player-id: " player-id))
  (let [new-player (swap! (get @ensemble player-id) set-first-note event-time transport.play-note/next-note)
        melody-event (play-first-melody-note new-player player-id event-time)
        ]

    (listeners-msg-new-segment (get-player-map player-id) 1)
    (send-message MSG-PLAYER-NEW-NOTE :player new-player :note-time event-time)
    (if (get-note-for-event melody-event)
      (check-note-off melody-event event-time))

    (sched-event 0
                 (get-player-val new-player "function") player-id
                 :time (+ event-time (get-dur-millis (get-dur-info-for-event melody-event))))
    )
  (log/data2 (log/format-msg "end first-note" "player-id: " player-id))
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
   :player-id player-no
   :prev-note-beat 0
   :prev-note-time 0
   }
  )

(defn init-ensemble
  []
  (reset! volume-adjust (min (/ 32 @setting/number-of-players) 1))
  (let [all-players (map create-player (range @setting/number-of-players))
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

  ;; (print-all-players)

  ;; Schedule first event for all players
  (dorun (map sched-event
              (repeat 0)
              (map get-player-val (get-ensemble) (repeat "function"))
              (map get-player-id (get-ensemble))))
  )
