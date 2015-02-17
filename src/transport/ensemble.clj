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

(ns transport.ensemble
  (:require
   [overtone.live :refer [apply-at ctl midi->hz node-live?]]
   [transport.behavior :refer [get-behavior-action get-behavior-player-id]]
   [transport.behaviors :refer [select-and-set-behavior-player-id]]
   [transport.dur-info :refer [get-dur-beats get-dur-millis]]
   [transport.ensemble-status :refer [update-ensemble-status]]
   [transport.instrument :refer [get-instrument has-release? play-instrument get-instrument-range-hi get-instrument-range-lo]]
   [transport.instrumentinfo :refer :all]
   [transport.melody :refer [next-melody]]
   [transport.melodyevent :refer :all]
   [transport.messages :refer :all]
   [transport.message-processor :refer [send-message register-listener unregister-listener]]
   [transport.player-copy :refer [player-copy-new-similar-info]]
   [transport.players :refer :all]
   [transport.schedule :refer [sched-event]]
   [transport.sc-instrument :refer [stop-instrument]]
   [transport.segment :refer [copy-following-info first-segment new-segment new-segment? get-contrasting-info-for-player]]
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
   :contrasting-info (get-contrasting-info-for-player (get-player contrast-player-id))
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
        (= (get-behavior-action (get-behavior player)) FOLLOW-PLAYER)
        (register-listener
         MSG-PLAYER-NEW-FOLLOW-INFO
         transport.players/new-change-follow-info-note-for-player
         {:change-player-id (get-behavior-player-id (get-behavior player))}
         :follow-player-id player-id
         )
        (= (get-behavior-action (get-behavior player)) SIMILAR-PLAYER)
        (register-listener
         MSG-PLAYER-NEW-SIMILAR-INFO
         transport.player-copy/player-copy-new-similar-info
         {:change-player-id (get-behavior-player-id (get-behavior player))}
         :follow-player-id player-id
         )
        (= (get-behavior-action (get-behavior player)) CONTRAST-PLAYER)
        (register-listener
         MSG-PLAYER-NEW-CONTRAST-INFO
         transport.ensemble/new-contrast-info
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
  [player]
  (let [prev-behavior (get-behavior player) ;; behavior before new segment
        ]
    (cond
     (= (get-behavior-action prev-behavior) FOLLOW-PLAYER)
     (unregister-listener
      MSG-PLAYER-NEW-FOLLOW-INFO
      transport.players/new-change-follow-info-note-for-player
      {:change-player-id (get-behavior-player-id prev-behavior)}
      :follow-player-id (get-player-id player)
      )

     (= (get-behavior-action prev-behavior) SIMILAR-PLAYER)
     (unregister-listener
      MSG-PLAYER-NEW-SIMILAR-INFO
      transport.player-copy/player-copy-new-similar-info
      {:change-player-id (get-behavior-player-id prev-behavior)}
      :follow-player-id (get-player-id player)
      )
     (= (get-behavior-action prev-behavior) CONTRAST-PLAYER)
     (unregister-listener
      MSG-PLAYER-NEW-CONTRAST-INFO
      transport.ensemble/new-contrast-info
      {:change-player-id (get-behavior-player-id prev-behavior)}
      :contrast-player-id (get-player-id player)
      )
     )
    )
  (new-segment player)
  )

(defn- update-melody
  [cur-melody next-melody-no new-melody-event]
  (if (= (count cur-melody) SAVED-MELODY-LEN)
    (do
      (assoc (dissoc cur-melody (- next-melody-no SAVED-MELODY-LEN))
        next-melody-no  new-melody-event)
      )
    (assoc cur-melody next-melody-no new-melody-event)
    )
  )

(defn- update-player-info
  [player event-time melody-event]
  (let [prev-note-beat (get-cur-note-beat player)
        cur-note-beat (cond (not (nil? (get-sync-beat-player-id player))) nil
                            (nil? (get-cur-note-beat player)) 0 ;; right after sync beat this will be nill so reset it
                            (not (nil? (get-dur-info-for-event melody-event))) (+ (get-cur-note-beat player) (get-dur-beats (get-dur-info-for-event melody-event)))
                            :else 0)
        prev-note-time event-time
        cur-note-time (+ prev-note-time (get-dur-millis (get-dur-info-for-event melody-event)))
        cur-melody (get-melody player)
        next-melody-no (if (empty? cur-melody)
                         1
                         (inc (get-last-melody-event-num-for-player player)))
        ;; if seg-start = 0 this is the begining of the segment, so
        ;; set seg-start to the time of this event - also send seg-start msg
        seg-start-time (if (= (get-seg-start player) 0)
                         event-time
                         (get-seg-start player))

        ]

    ;; If current segment is over, sched next event with a new segment
    ;; else sched event with current segment information
    (if (< (+ seg-start-time (get-seg-len player)) event-time)
      (assoc player
        :cur-note-beat 0
        :cur-note-time cur-note-time
        :melody (update-melody cur-melody next-melody-no melody-event)
        :prev-note-beat prev-note-beat
        :prev-note-time prev-note-time
        :seg-start seg-start-time
        :sync-beat-player-id nil
        )
      (assoc player
        :cur-note-beat cur-note-beat
        :cur-note-time cur-note-time
        :melody (update-melody cur-melody next-melody-no melody-event)
        :prev-note-beat prev-note-beat
        :prev-note-time prev-note-time
        :seg-start seg-start-time
        :sync-beat-player-id nil
        )
      ))
  )

(defn check-note-out-of-range
  [player-id melody-event]
  ;; Throw exception if note is out of instrument's range
  (if (or (> (:note melody-event) (get-range-hi-for-inst-info (get-instrument-info-for-event melody-event)))
          (< (:note melody-event) (get-range-lo-for-inst-info (get-instrument-info-for-event melody-event))))
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
        (print-msg "stop-melody-note" "player-id: " player-id " note: " (get-note-for-event melody-event) " event-time: " (get-note-event-time-for-event melody-event))
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
        prior-melody-event (get-melody-event-num player (- last-melody-event-num 20))]
    (if (and (> (get-last-melody-event-num-for-player player) 20)
             (node-live? (get-sc-instrument-id prior-melody-event))
             (not= (get-sc-instrument-id prior-melody-event)
                   (get-sc-instrument-id (get-melody-event-num player last-melody-event-num)))
             (not= (get-sc-instrument-id prior-melody-event)
                   (get-sc-instrument-id (get-melody-event-num player (- last-melody-event-num 1))))
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
  "Returns true if this event is a
   new segment in the following player

   player - map for the player to check
   melody event - emlody event to check or player's last melody event if omitted"
  [player & {:keys [melody-event increment]
             :or {melody-event (get-last-melody-event player)
                  increment 0}}]

  (comment
    *******************************
    (if (and (get-change-follow-info-note-for-event melody-event)
             (get-follow-note-for-event melody-event)
             (> (get-follow-note-for-event melody-event) (get-change-follow-info-note-for-event melody-event)))
      (do
        (binding [*out* *err*]
          (print-msg "new-segment-for-following-player?" "!!!!! CHANGE-FOLLOW-INFO-NOTE < FOLLOW-NOTE !!!!!" )
          (print-player player)
          )
        (throw (Throwable. "SMALL CHANGE FOLLOW INFO NOTE"))
        ))
    *******************************
    )
  (and (get-next-change-follow-info-note player)
       (>= (+ (get-follow-note-for-event melody-event) increment) (get-next-change-follow-info-note player)))
  )

(defn- check-note-off
  "Schedule a note off for the last note played
    by player if necessary and/or possible

   player - map for the current player"
  [player event-time]

  (let [cur-melody-event (get-last-melody-event player)
        inst-has-release? (has-release? (get-instrument-info cur-melody-event))
        ;; all notes without release (AD or NE) are not articulated (stopped)
        articulate? (if inst-has-release?
                      (articulate-note? cur-melody-event event-time)
                      false
                      )
        ]
    (cond articulate?
          (do
            ;; (print-msg "check-note-off" "player-id: " (get-player-id player) " note: " (get-note-for-event cur-melody-event))
            (apply-at (+ event-time
                         (- (get-dur-millis (get-dur-info-for-event cur-melody-event))
                            (get-release-millis-for-inst-info (get-instrument-info-for-event cur-melody-event))
                            ))
                      stop-melody-note
                      [cur-melody-event (get-player-id player)]))
          ;; stop prev note when it is short (not articulate?) and starting a new segment with a note
          ;; with a release
          (and inst-has-release? (new-segment? player))
          (apply-at (+ event-time (get-dur-millis (get-dur-info-for-event cur-melody-event)))
                    stop-melody-note
                    [cur-melody-event (get-player-id player)])
          )
  ))

(defn play-melody
  "Gets the note to play now and plays it (if it is not a rest)
   Checks if the current segment is done, and if so
   sets up a new one.
   Then schedules the next note to play

   player - map for the current player
   player-id - the id of the current player
   event-time - time this note event was scheduled for"
  [player player-id event-time]

  ;; (println)
  ;; (print-msg "play-melody"  "player-id: " player-id " event-time: " event-time " current time: " (System/currentTimeMillis))
  (let [last-melody-event-num (get-last-melody-event-num-for-player player)
        last-melody-event (get-last-melody-event player)
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
        new-seg? (>= event-time (+ (get-seg-start player) (get-seg-len player)))
        upd-seg-player (cond new-seg?
                             (update-player-with-new-segment player)
                             (new-segment-for-following-player? player
                                                                :melody-event last-melody-event
                                                                :increment 1)
                             (update-player-follow-info player (inc last-melody-event-num))
                             :else
                             player
                             )
        melody-event (next-melody upd-seg-player event-time)
        melody-event-note (get-note-for-event melody-event)
        melody-dur-millis (get-dur-millis (get-dur-info-for-event melody-event))
        sc-instrument-id (if (not (nil? melody-event-note))
                           (cond
                            (or articulate?
                                new-seg?
                                (and (not inst-has-release?)
                                     (new-segment-for-following-player? upd-seg-player :melody-event melody-event)
                                     )
                                )
                            ((get-instrument-for-inst-info (get-instrument-info-for-event melody-event))
                             (midi->hz (get-note-for-event melody-event))
                             (get-volume-for-event melody-event)
                             )
                            (new-segment-for-following-player? upd-seg-player :melody-event melody-event)
                            (do
                              (stop-melody-note last-melody-event player-id)
                              ((get-instrument-for-inst-info (get-instrument-info-for-event melody-event))
                               (midi->hz (get-note-for-event melody-event))
                               (get-volume-for-event melody-event)
                               )
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
        upd-player (update-player-info upd-seg-player event-time (set-sc-instrument-id-and-note-play-time
                                                                  melody-event
                                                                  sc-instrument-id
                                                                  note-play-time))
        new-player (if (new-segment-for-following-player? upd-seg-player :melody-event melody-event)
                     (assoc upd-player :change-follow-info-notes (subvec (get-change-follow-info-notes upd-player) 1))
                     upd-player
                     )
        ]
    (if (not (nil? melody-event-note))
      ;; if about to play a note, check range
      (check-note-out-of-range player-id melody-event)
      ;; if about to rest, and
      ;; this rest is not a new seg for FOLLOWING PLAYER (this is handled in check-note-off)
      ;; make sure prior note is off
      (if (and (not articulate?)
               inst-has-release?
               )
        (stop-melody-note last-melody-event player-id)
        )
      )

    (if (nil? melody-dur-millis)
      (print-msg "play-melody" "MELODY EVENT :DUR IS NILL !!!!"))

    new-player
    ))

(defn next-note
  [player-id event-time]
  (let [new-player (swap! (get @ensemble player-id) play-melody player-id event-time)
        melody-event (get-last-melody-event new-player)
        melody-event-note (get-note-for-event melody-event)
        melody-dur-millis (get-dur-millis (get-dur-info-for-event melody-event))
        ]
    (if melody-event-note
      (check-note-off new-player event-time)
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
   Checks if the current segment is done, and if so
   sets up a new one.
   Then schedules the next note to play

   This function is only used for the very first note an instrument plays.
   For notes after the first, this function sets the player to use play-melody.

   player - map for the current player
   player-id - the id of the current player
   event-time - time this note event was scheduled for"
  [player player-id event-time]

  (let [melody-event (next-melody player event-time)
        sc-instrument-id (if (not (nil? (:note melody-event)))
                             ((get-instrument-for-inst-info (get-instrument-info-for-event melody-event))
                              (midi->hz (get-note-for-event melody-event))
                              (get-volume-for-event melody-event)
                              )
                             nil
                             )
        note-play-time (max (System/currentTimeMillis) event-time)
        upd-player (update-player-info (set-function player transport.ensemble/next-note)
                                       event-time
                                       (set-sc-instrument-id-and-note-play-time melody-event
                                                                                sc-instrument-id
                                                                                note-play-time))
        ]

    (if (not (nil? (:note melody-event)))
      ;; if about to play a note, check range
      (check-note-out-of-range player-id melody-event))
    upd-player
    )
  )

(defn first-note
  [player-id event-time]
  (let [new-player (swap! (get @ensemble player-id) play-first-melody-note player-id event-time)
        melody-event (get-last-melody-event new-player)
        melody-dur-millis (get-dur-millis (get-dur-info-for-event melody-event))
        release-time (-
                      (+ event-time melody-dur-millis)
                      (if (nil? (:note melody-event))
                        0
                        (get-actual-release-dur-millis (get-instrument-info new-player) melody-dur-millis)) )
        ]

    (send-message MSG-PLAYER-NEW-NOTE :player new-player :note-time event-time)
    (if (get-note-for-event melody-event)
      (check-note-off new-player event-time))

    (sched-event 0
                 (get-player-val new-player "function") player-id
                 :time release-time)
    )
  )

(defn create-player
  [player-no]
  (first-segment {:cur-note-beat 0
                  :cur-note-time 0
                  :function transport.ensemble/first-note
                  :melody {}
                  :player-id player-no
                  :prev-note-beat 0
                  :prev-note-time 0
                  :sync-beat-player-id nil
                  }))

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
        ]
    (reset-players)
    (swap! ensemble conj final-players)
    )

  (dorun (map print-player (get-players)))

  ;; Schedule first event for all players
  (dorun (map sched-event
              (repeat 0)
              (map get-player-val (get-players) (repeat "function"))
              (map get-player-id (get-players))))
  )
