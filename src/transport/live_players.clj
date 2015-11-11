;    Copyright (C) 2015  Joseph Fosco. All Rights Reserved
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

(ns transport.live-players

  (:use [overtone.live]
   )
  (:require
   [overtone.live :refer [midi->hz]]
   [overtone.midi :as midi]
   [transport.constants :refer [SAVED-MELODY-LEN]]
   [transport.instrument :refer [get-instrument-info-for-name]]
   [transport.instrumentinfo :refer [get-instrument-for-inst-info]]
   [transport.melody.liveplayermelodyevent :refer [create-live-player-melody-event get-live-player-note
                                                   get-live-player-sc-instrument-id get-live-player-start-time]]
   [transport.sc-instrument :refer [stop-instrument]]
   [transport.settings :refer [get-setting number-of-live-players]]
   [transport.util.utils :refer [get-max-map-key print-msg]]
   )
  )

(def live-players (atom {}))
(def live-player-melodies (atom {}))

(defn get-live-player
 [live-player-id]
 (deref (get @live-players live-player-id))
 )

(defn get-instrument-info-for-live-player
 [live-player]
 (:instrument-info live-player)
 )

(defn get-instrument-info-for-live-player-id
 [live-player-id]
 (get-instrument-info-for-live-player (get-live-player live-player-id))
 )

(defn get-instrument-for-live-player-id
 [live-player-id]
 (get-instrument-for-inst-info (get-instrument-info-for-live-player-id live-player-id))
 )

(defn get-player-for-midi-event
 [midi-event]
 0
 )

(defn get-last-melody-event-num-for-live-player
  [live-player-id]
  (get-max-map-key (:melody (deref (get @live-player-melodies live-player-id))))
  )

(defn new-melody-event
  [live-player-id midi-event sc-instrument-id]
  (create-live-player-melody-event :note (:note midi-event)
                                   :dur-millis nil
                                   :start-time (:timestamp midi-event)
                                   :volume (:velocity midi-event)
                                   :player-id live-player-id
                                   :sc-instrument-id sc-instrument-id
                                   )
  )

(defn update-melody-list
  "If current melody info = SAVED-MELODY-LEN, remove the oldest melody
   event and add the new melody event, otherwise, just add the new
   melody event

   cur-melody-info - current melody info for live-player
   live-player-id - id of live-player that is haveing
                    it's melody updated
   new-melody-event - the melody event to add"
  [cur-melody-info live-player-id new-melody-event]
  (let [new-melody-event-num (inc (get-last-melody-event-num-for-live-player live-player-id))
        new-melody (assoc
                       (if (= (count (:melody cur-melody-info)) SAVED-MELODY-LEN)
                         (dissoc (:melody cur-melody-info) (- new-melody-event-num SAVED-MELODY-LEN))
                         (:melody cur-melody-info))
                     new-melody-event-num
                     new-melody-event)]
    (assoc cur-melody-info :melody new-melody)
    )
  )

(defn next-note
  "Plays note and adds to live-players-melody"
  [midi-event live-player-id]
  (let  [instrument (get-instrument-for-live-player-id live-player-id)
         sc-instrument-id (if instrument
                            (instrument
                             (midi->hz (:note midi-event))
                             (float (/ (:velocity midi-event) 127))
                             )
                            nil)
         ]
    (swap! (get @live-player-melodies live-player-id)
           update-melody-list
           live-player-id
           (new-melody-event live-player-id midi-event sc-instrument-id)
           ))
  )

(defn- get-melody-event-for-note
  "Finds the last melody event in the live-players where
   the melody event note is equal to the midi-event note.
   Returns a map containing the melody event number (key)
   and the melody-event (value)

   cur-melody-info - melody map for live-player-id
   midi-event - midi-event to match note with
   live-player-id - live player melody to check"
  [cur-melody-info midi-event live-player-id]
  (let [last-melody-event-num (get-last-melody-event-num-for-live-player live-player-id)]
    (loop [melody-event-num last-melody-event-num]
      (when (< melody-event-num 1)
        (throw (Throwable. "NoNoteToStop"))
        )
      (let [melody-event (get (:melody cur-melody-info) melody-event-num)]
        (if (= (:note midi-event) (get-live-player-note melody-event))
          {melody-event-num melody-event}
          (recur (dec melody-event-num))
          )
        )
      )
    )
  )

(defn set-note-off
  "Stop current note and and set the duration of the current note's melody event

  cur-melody-info - all melody info for live-player
  midi-event      - the note-off midi-event
  live-player-id  - the id of the live-player"
  [cur-melody-info midi-event live-player-id]
  (let [melody-event-info (get-melody-event-for-note cur-melody-info midi-event live-player-id)
        melody-event (first (vals melody-event-info))
        melody-event-num (first (keys melody-event-info))
        dur (quot (- (:timestamp midi-event) (get-live-player-start-time melody-event)) 1000)
        new-melody-event (assoc melody-event :dur-millis dur)
        ]
    (stop-instrument (get-live-player-sc-instrument-id melody-event))
    (assoc cur-melody-info :melody (assoc (:melody cur-melody-info) melody-event-num new-melody-event))
    )
  )

(defn note-end
  [midi-event live-player-id]
  (swap! (get @live-player-melodies live-player-id)
         set-note-off
         midi-event
         live-player-id
         )
  )

(defn process-note
  [midi-event]
  (println "****************  NOTE RECEIVED: " (:status midi-event) (:note midi-event) " %%%%%%%%%%%%%%%%%%%%%%%")
    (let [live-player-id (get-player-for-midi-event midi-event)]
      (cond (= (:status midi-event) :note-on)
            (next-note midi-event live-player-id)
            (= (:status midi-event) :note-off)
            (note-end midi-event live-player-id)
            )
      )
  )

(defn create-live-player
  [player-no]
  (let [instrument-info (try
                          (get-instrument-info-for-name (get-setting (str "midi-instrument-" player-no)))
                          (catch Exception e
                            nil)
                          )
        ]

    (atom {:function process-note
           :live-player-id player-no
           :midi-port (deref (eval (symbol (str "transport.settings/" "midi-port-" player-no))))
           :midi-channel (deref (eval (symbol (str "transport.settings/" "midi-channel-" player-no))))
           :instrument-info instrument-info
           })
    )
  )

(defn set-up-midi
  [midi-port fnc]
  (midi/midi-handle-events (midi/midi-in midi-port) fnc)
  )

(defn initial-live-player-melody
  [live-player-no]
  {
   :live-player-id live-player-no
   :melody {}
   }
  )

(defn init-live-players
  []
  (when (> @number-of-live-players 0)
    (reset! live-players (zipmap
                          (range @number-of-live-players)
                          (map create-live-player (range @number-of-live-players))
                          ))

    (let [players (map deref (vals @live-players))
          midi-info (for [port (map :midi-port players) fnc (map :function players)] [port fnc])]
      (dorun (map set-up-midi (map first midi-info) (map second midi-info)))
      )
    ;; initialize live-players-melodies
    (let [live-player-ids (vec (map get (map deref (vals @live-players)) (repeat :live-player-id)))]
      (reset! live-player-melodies
              (zipmap live-player-ids
                      (map atom (map initial-live-player-melody live-player-ids))
                      ))
      )
    )
  )
