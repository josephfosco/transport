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

(ns transport.ensemble
  (:require
   [overtone.live :refer [apply-at midi->hz]]
   [transport.behavior :refer [get-behavior-action get-behavior-player-id]]
   [transport.behaviors :refer [get-behavior-player-id-for-player select-and-set-behavior-player-id]]
   [transport.dur-info :refer [get-dur-beats get-dur-millis]]
   [transport.ensemble-status :refer [update-ensemble-status]]
   [transport.instrument :refer [get-instrument play-instrument get-instrument-range-hi get-instrument-range-lo stop-last-note]]
   [transport.instrumentinfo :refer :all]
   [transport.melody :refer [next-melody]]
   [transport.melodyevent :refer :all]
   [transport.messages :refer :all]
   [transport.message-processor :refer [send-message register-listener unregister-listener]]
   [transport.player-copy :refer [player-copy-new-similar-info]]
   [transport.players :refer :all]
   [transport.schedule :refer [sched-event]]
   [transport.segment :refer [copy-following-info first-segment new-segment get-contrasting-info-for-player]]
   [transport.settings :refer :all]
   [transport.util :refer :all]
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
        (= (get-behavior-action-for-player player) FOLLOW-PLAYER)
        (register-listener
         MSG-PLAYER-NEW-FOLLOW-INFO
         transport.players/new-change-follow-info-note-for-player
         {:change-player-id (get-behavior-player-id-for-player player)}
         :follow-player-id player-id
         )

        (= (get-behavior-action-for-player player) SIMILAR-PLAYER)
        (register-listener
         MSG-PLAYER-NEW-SIMILAR-INFO
         transport.player-copy/player-copy-new-similar-info
         {:change-player-id (get-behavior-player-id-for-player player)}
         :follow-player-id player-id
         )
        (= (get-behavior-action-for-player player) CONTRAST-PLAYER)
        (register-listener
         MSG-PLAYER-NEW-CONTRAST-INFO
         transport.ensemble/new-contrast-info
         {:change-player-id (get-behavior-player-id-for-player player)}
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
        cur-note (get-note-for-event melody-event)
        cur-note-beat (cond (not (nil? (get-sync-beat-player-id player))) nil
                            (nil? (get-cur-note-beat player)) 0 ;; right after sync beat this will be nill so reset it
                            (not (nil? (get-dur-info-for-event melody-event))) (+ (get-cur-note-beat player) (get-dur-beats (get-dur-info-for-event melody-event)))
                            :else 0)
        prev-note-time event-time
        cur-note-time (+ prev-note-time (get-dur-millis (get-dur-info-for-event melody-event)))
        cur-melody (get-melody player)
        next-melody-no (if (empty? cur-melody)
                         1
                         (inc (reduce max (keys cur-melody))))
        ;; if seg-start = 0 this is the begining of the segment, so
        ;; set seg-start to the time of this event - also send seg-start msg
        seg-start-time (if (= (get-seg-start player) 0)
                         (do
                           ;; send msgs and set listeners for new segmwnt
                           (listeners-msg-new-segment player next-melody-no)
                           event-time)
                         (get-seg-start player))

        ]

    ;; If current segment is over, sched next event with a new segment
    ;; else sched event with current segment information
    (if (< (+ seg-start-time (get-seg-len player)) event-time)
      (update-player-with-new-segment
       (assoc player
         :melody (update-melody cur-melody next-melody-no melody-event)
         :cur-note-beat 0
         :cur-note-time cur-note-time
         :prev-note-beat prev-note-beat
         :prev-note-time prev-note-time
         ))
      (assoc player
        :cur-note-beat cur-note-beat
        :cur-note-time cur-note-time
        :last-pitch (if (not (nil? cur-note)) cur-note (get-last-pitch player))
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

(defn play-next-note
  [sc-instrument-id player-id event-time]
  (print-msg "play-next-note" "player-id: " player-id " current time: " (System/currentTimeMillis))
  (play-instrument sc-instrument-id)
  (send-message MSG-PLAYER-NEW-NOTE :player (get-player player-id) :note-time event-time)
  )

(defn get-actual-release-dur-millis
  [inst-info dur-millis]
  (let [release-dur (get-release-millis-for-inst-info inst-info)]
    (if (< (+ release-dur 20) dur-millis)
      release-dur
      0))
  )

(defn- articulate-next-note?
  [melody-event event-time]
  (let [dur-millis (get-dur-millis (get-dur-info-for-event melody-event))
        release-dur (get-actual-release-dur-millis
                     (get-instrument-info-for-event melody-event)
                     dur-millis
                     )
        ]
    (if (and (> release-dur 0)
             (> (- dur-millis release-dur) 20)
             (> (- event-time (get-note-play-time-for-event melody-event)) 20)
             )
      true
      false)
    )
  )

(defn play-melody
  "Gets the note to play now and plays it (if it is not a rest)
   Checks if the current segment is done, and if so
   sets up a new one.
   Then schedules the next note to play

   player - map for the current player
   event-time - time this note event was scheduled for"
  [player-id event-time]

  (println)
  (print-msg "play-melody"  "player-id: " player-id " current time: " (System/currentTimeMillis))
  (let [player (get-player player-id)
        last-melody-event (get-last-melody-event player)
        articulate? (if last-melody-event (articulate-next-note? last-melody-event event-time) false)
        ]
    (print-msg "play-melody" "articulate?: " articulate?)
    (if (and last-melody-event articulate?)
      (stop-last-note player))
    (let [melody-event (next-melody player event-time)
          melody-dur-millis (get-dur-millis (get-dur-info-for-event melody-event))
          ]

      (if (not (nil? (:note melody-event)))
        ;; if about to play a note, check range
        (check-note-out-of-range player-id melody-event)
        ;; else if about to rest, stop previous note
        (if (not articulate?) (apply-at (+ event-time 20) stop-last-note [player]))
        )

      (let [sc-instrument-id (if (not (nil? (:note melody-event)))
                               (if (and last-melody-event articulate?)
                                 ((get-instrument player)
                                  (midi->hz (get-note-for-event melody-event))
                                  (get-volume-for-event melody-event)
                                  (get-release-dur-for-inst-info (get-instrument-info player))
                                  )
                                 (get-sc-instrument-id last-melody-event)
                                 )
                               nil
                               )
            next-note-time (if (nil? last-melody-event)
                             event-time
                             (+ event-time (get-actual-release-dur-millis
                                            (get-instrument-info-for-event last-melody-event)
                                            (get-dur-millis (get-dur-info-for-event last-melody-event))
                                            )))
            ]
        (if sc-instrument-id
          (sched-event 0 play-next-note (list sc-instrument-id player-id) :time next-note-time )
          )
        (if (nil? melody-dur-millis)
          (print-msg "play-melody" "MELODY EVENT :DUR IS NILL !!!!"))

        (let [note-play-time (max (System/currentTimeMillis) next-note-time)
              upd-player (update-player-info player event-time (set-sc-instrument-id-and-note-play-time
                                                                melody-event
                                                                sc-instrument-id
                                                                note-play-time))
              cur-change-follow-info-note (get-change-follow-info-note upd-player)
              release-time (-
                            (+ next-note-time melody-dur-millis)
                            (if (nil? (:note melody-event))
                              0
                              (get-actual-release-dur-millis (get-instrument-info upd-player) melody-dur-millis)) )
              ]
          ;; check if player is following another player and following player changed segments
          ;; if it did, update this player with the new segment info from the following player
          ;; if this player is on the note where the following player changed segments
          (if (and
               cur-change-follow-info-note
               (>= (inc (get-follow-note-for-event (get-last-melody-event upd-player))) cur-change-follow-info-note)
               )
            ;; next note is the note that the FOLLOW player changed segments
            (update-player-and-follow-info upd-player)
            (update-player upd-player)
            )
          (sched-event 0
                       (get-player-val upd-player "function") player-id
                       :time release-time)
          (print-msg "play-melody" "current-time " (System/currentTimeMillis) " next-note-time: " next-note-time " melody-dur-millis " melody-dur-millis " release-time " release-time)
          (print-msg "play-melody" "last-melody-event-num: " (get-last-melody-event-num-for-player upd-player))
          )))
  ))

(defn create-player
  [player-no]
  (first-segment {:cur-note-beat 0
                  :cur-note-time 0
                  :function transport.ensemble/play-melody
                  :melody {}
                  :player-id player-no
                  :prev-note-beat 0
                  :prev-note-time 0
                  :sync-beat-player-id nil
                  }))

(defn init-ensemble
  []
  (let [all-players (map create-player (range @number-of-players))]
    (reset-players)
    (swap! PLAYERS conj (zipmap (map get all-players (repeat :player-id)) all-players))
    )

  ;; set the :behavior :player-id for all players that are FOLLOWing, SIMILARing or CONTRASTing other players
  (let [final-players
        (zipmap
         (keys @PLAYERS)
         (map assoc (vals @PLAYERS) (repeat :behavior) (map select-and-set-behavior-player-id (vals @PLAYERS))))
        ]
    (reset-players)
    (swap! PLAYERS conj final-players)
    )

  (init-players)

  (dorun (map print-player (get-players)))

  ;; Schedule first event for all players
   (dorun (map sched-event (repeat 0) (map get-player-val (get-players) (repeat "function")) (map get-player-id (get-players))))
  )
