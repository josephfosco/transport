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
   [transport.instrument :refer [get-instrument-info-for-name]]
   [transport.instrumentinfo :refer [get-instrument-for-inst-info]]
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

(defn get-sc-instrument-id
  [live-player-melody-event]
  (:sc-instrument-id live-player-melody-event)
  )

(defn get-player-for-midi-event
 [midi-event]
 0
 )

(defn get-last-melody-event-num-for-live-player
  [live-player-id]
  (get-max-map-key (:melody (deref (get @live-player-melodies live-player-id))))
  )

(defn create-melody-event
  [live-player-id midi-event sc-instrument-id]
  {
   :note (:note midi-event)
   :dur-millis nil
   :start-time (:timestamp midi-event)
   :volume (:velocity midi-event)
   :player-id live-player-id
   :sc-instrument-id sc-instrument-id
   }
  )

(defn add-melody-event
  [cur-melody-info live-player-id new-melody-event]
  (let [new-melody (assoc (:melody cur-melody-info)
                     (inc (get-last-melody-event-num-for-live-player live-player-id))
                     new-melody-event)]
    (assoc cur-melody-info :melody new-melody)
    )
  )

(defn add-note-duration
  "Stop current note and and the duration to the current note's melody event

  cur-melody-info - all melody info for live-player
  midi-event      - the note-off midi-event
  live-player-id  - the id of the live-player"
  [cur-melody-info midi-event live-player-id]
  (let [last-melody-event-num (get-last-melody-event-num-for-live-player live-player-id)
        melody-event (get (:melody cur-melody-info) last-melody-event-num)
        dur (quot (- (:timestamp midi-event) (:start-time melody-event)) 1000)
        new-melody-event (assoc melody-event :dur-millis dur)
        ]
    (stop-instrument (get-sc-instrument-id melody-event))
    (assoc cur-melody-info :melody (assoc (:melody cur-melody-info) last-melody-event-num new-melody-event))
    )
  )

(defn add-note-off
  [cur-melody-info new-melody-event midi-event live-player-id]
  (let [melody-with-dur (add-note-duration cur-melody-info midi-event live-player-id)
        new-melody-info  (add-melody-event melody-with-dur new-melody-event live-player-id)]
       new-melody-info
       )
  )

(defn next-note
  [midi-event live-player-id]
  (let  [sc-instrument-id ((get-instrument-for-live-player-id live-player-id)
                           (midi->hz (:note midi-event))
                           (float (/ (:velocity midi-event) 127))
                           )
         ]
    (swap! (get @live-player-melodies live-player-id)
           add-melody-event
           live-player-id
           (create-melody-event live-player-id midi-event sc-instrument-id)
           ))
  )

(defn note-end
  [midi-event live-player-id]
  (comment
    (swap! (get @live-player-melodies live-player-id)
           add-note-off
           (create-melody-event live-player-id midi-event)
           midi-event
           live-player-id
           ))
  (swap! (get @live-player-melodies live-player-id)
         add-note-duration
         midi-event
         live-player-id
         )
  )

(defn process-note
  [midi-event]
  (println "****************  NOTE RECEIVED ******************")
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
