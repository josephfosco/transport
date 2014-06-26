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
   [overtone.live]
   [transport.behavior :refer [get-behavior-action get-behavior-player-id]]
   [transport.behaviors :refer [get-behavior-action-for-player get-behavior-player-id-for-player select-and-set-behavior-player-id]]
   [transport.debug :refer [debug-run1]]
   [transport.ensemble-status :refer [update-ensemble-status]]
   [transport.instrument :refer [get-instrument play-instrument]]
   [transport.melody :refer [next-melody]]
   [transport.melodyevent :refer [get-dur-info-for-event get-dur-millis get-follow-note-for-event get-note-for-event get-volume-for-event]]
   [transport.messages :refer :all]
   [transport.message-processor :refer [send-message register-listener unregister-listener]]
   [transport.player-copy :refer [player-copy-new-complement-info]]
   [transport.players :refer :all]
   [transport.rhythm :refer [get-beats]]
   [transport.schedule :refer [sched-event]]
   [transport.segment :refer [copy-following-info first-segment new-segment get-contrasting-info-for-player]]
   [transport.settings :refer :all]
   )
  (:import transport.behavior.Behavior)
  )

(defn new-contrast-info
  [& {:keys [change-player-id follow-player-id originator-player-id melody-no]}]
  (send PLAYERS
        set-new-contrast-info
        change-player-id
        follow-player-id
        originator-player-id
        (get-contrasting-info-for-player (get-player follow-player-id))
        )
  )

(defn- listeners-msg-new-segment
  "Called first time player plays a note in a new segment.
   Sends a message that it is in a new segment, and
   if the player is FOLLOWing, COMPLEMENTing, or CONTRASTing
   it will listen for new segmemnts in the player it is
   FOLLOWing etc...

   player - the player starting a new segment
   melody-no - the key of the melody event that is being played"
  [player melody-no]
  (println "ensemble.clj - listeners-msg-new-segment melody-no:" melody-no)
  (let [player-id (get-player-id player)]
       (send-message MSG-PLAYER-NEW-SEGMENT :change-player-id player-id :originator-player-id player-id)
       (send-message MSG-PLAYER-NEW-FOLLOW-INFO :change-player-id player-id :originator-player-id player-id :melody-no melody-no)
       (send-message MSG-PLAYER-NEW-COMPLEMENT-INFO :change-player-id player-id :originator-player-id player-id)
       (send-message MSG-PLAYER-NEW-CONTRAST-INFO :change-player-id player-id :originator-player-id player-id)

       (cond
        (= (get-behavior-action-for-player player) FOLLOW)
        (register-listener
         MSG-PLAYER-NEW-FOLLOW-INFO
         transport.players/new-change-follow-info-note-for-player
         {:change-player-id (get-behavior-player-id-for-player player)}
         :follow-player-id player-id
         )

        (= (get-behavior-action-for-player player) COMPLEMENT)
        (register-listener
         MSG-PLAYER-NEW-COMPLEMENT-INFO
         transport.player-copy/player-copy-new-complement-info
         {:change-player-id (get-behavior-player-id-for-player player)}
         :follow-player-id player-id
         )
        (= (get-behavior-action-for-player player) CONTRAST)
        (register-listener
         MSG-PLAYER-NEW-CONTRAST-INFO
         transport.ensemble/new-contrast-info
         {:change-player-id (get-behavior-player-id-for-player player)}
         :follow-player-id player-id
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
     (= (get-behavior-action prev-behavior) FOLLOW)
     (unregister-listener
      MSG-PLAYER-NEW-FOLLOW-INFO
      transport.players/new-change-follow-info-note-for-player
      {:change-player-id (get-behavior-player-id prev-behavior)}
      :follow-player-id (get-player-id player)
      )

     (= (get-behavior-action prev-behavior) COMPLEMENT)
     (unregister-listener
      MSG-PLAYER-NEW-COMPLEMENT-INFO
      transport.player-copy/player-copy-new-complement-info
      {:change-player-id (get-behavior-player-id prev-behavior)}
      :follow-player-id (get-player-id player)
      )
     (= (get-behavior-action prev-behavior) CONTRAST)
     (unregister-listener
      MSG-PLAYER-NEW-CONTRAST-INFO
      transport.ensemble/new-contrast-info
      {:change-player-id (get-behavior-player-id prev-behavior)}
      :follow-player-id (get-player-id player)
      )
     )
    )
  (new-segment player)
  )

(defn- update-player-info
  [player event-time melody-event]
  (let [prev-note-beat (:cur-note-beat player)
        cur-note (get-note-for-event melody-event)
        cur-note-beat (if (not (nil? (:dur-info melody-event)))
                        (+ (:cur-note-beat player) (get-beats (:dur-info melody-event)))
                        0)
        cur-melody (get-melody player)
        next-melody-no (if (> (count cur-melody) 0)
                         (inc (reduce max (keys cur-melody)))
                         1)
        ;; if seg-start = 0 this is the begining of the segment, so
        ;; set seg-start to the time of this event - also send seg-start msg
        seg-start-time (if (= (:seg-start player) 0)
                         (do
                           ;; send msgs and set listeners for new segmwnt
                           (listeners-msg-new-segment player next-melody-no)
                           event-time)
                         (:seg-start player))

        ]
    ;; If current segment is over, sched next event with a new segment
    ;; else sched event with current segment information
    (if (< (+ seg-start-time (:seg-len player)) event-time)
      (update-player-with-new-segment
       (assoc player
         :cur-note-beat cur-note-beat
         :prev-note-beat prev-note-beat))
      (assoc player
        :cur-note-beat cur-note-beat
        :last-pitch (if (not (nil? cur-note)) cur-note (get-last-pitch player))
        :melody (if (= (count cur-melody) SAVED-MELODY-LEN)
                  (do
                    (assoc (dissoc cur-melody (- next-melody-no SAVED-MELODY-LEN))
                      next-melody-no  melody-event)
                    )
                  (assoc cur-melody next-melody-no melody-event)
                  )
        :prev-note-beat prev-note-beat
        :seg-start seg-start-time
        )
      ))
  )

(defn play-melody
  "Gets the note to play now and plays it (if it is not a rest)
   Checks if the current segment is done, and if so
   sets up a new one.
   Then schedules the next note to play

   player - map for the current player
   event-time - time this note event was scheduled for"
  [player-id event-time]
  (println "ensemble.clj - play-melody - player-id:" player-id)
  (let [
        player (get-player player-id)
        melody-event (next-melody player event-time)
        melody-dur-millis (get-dur-millis (get-dur-info-for-event melody-event))
        ]

    (if (not (nil? (:note melody-event)))
      (play-instrument player (:note melody-event) melody-dur-millis (get-volume-for-event melody-event)))
    (if (nil? melody-dur-millis)
      (println "MELODY EVENT :DUR IS NILL !!!!"))
    (let [upd-player (update-player-info player event-time melody-event)]
      (let [cur-change-follow-info-note (get-change-follow-info-note player)]
        (if cur-change-follow-info-note
          (let [follow-note (get-follow-note-for-event (get-last-melody-event player))]
            (if (nil? follow-note)
              (do
                ;; this is the first note player is FOLLOWing
                (println "ensemble.clj - play-melody - follow 1 cur-change-follow-info-note:" cur-change-follow-info-note "follow-note:" follow-note)
                (update-player-and-follow-info upd-player)
                )
              (if (>= (inc follow-note) cur-change-follow-info-note)
                (do
                  (println "ensemble.clj - play-melody - follow 2 cur-change-follow-info-note:" cur-change-follow-info-note "follow-note:" follow-note)
                  (update-player-and-follow-info upd-player)
                  )
                (update-player upd-player)))
            )
          (update-player upd-player)
          ))
      (sched-event melody-dur-millis (get-function upd-player) (get-player-id upd-player))
      (send-message MSG-PLAYER-NEW-NOTE :player upd-player :note-time event-time)
      ))
  )

(defn create-player
  [player-no]
  (first-segment {:cur-note-beat 0
                  :function transport.ensemble/play-melody
                  :melody {}
                  :player-id player-no
                  :prev-note-beat 0}))

(defn init-ensemble
  []
  (let [all-players (map create-player (range @number-of-players))]
    (reset-players)
    (send PLAYERS conj (zipmap (map get all-players (repeat :player-id)) all-players))
    (await PLAYERS)
    )

  ;; set the :behavior :player-id for all players that are FOLLOWing,COMPLEMENTing or CONTRASTing
  (let [final-players
        (zipmap
         (keys @PLAYERS)
         (map assoc (vals @PLAYERS) (repeat :behavior) (map select-and-set-behavior-player-id (vals @PLAYERS))))
        ]
    (reset-players)
    (send PLAYERS conj final-players)
    (await PLAYERS)
    )

  (init-players)

  (dorun (map print-player (get-players)))

  ;; Schedule first event for all players
   (dorun (map sched-event (repeat 0) (map get-function (get-players)) (map get-player-id (get-players))))
  )
