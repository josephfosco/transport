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
  (:require [overtone.midi :as midi]
            [transport.settings :refer [number-of-live-players]]
            [transport.util.utils :refer [get-max-map-key print-msg]]
            )
  )

(def live-players (atom {}))
(def live-player-melodies (atom {}))

(defn get-last-melody-event-num-for-live-player
  [live-player-id]
  (get-max-map-key (:melody (deref (get @live-player-melodies live-player-id))))
  )

(defn create-melody-event
  [live-player-id midi-event]
  {
   :note (:note midi-event)
   :dur-millis nil
   :start-time (:timestamp midi-event)
   :volume (:velocity midi-event)
   :player-id live-player-id
   }
  )

(defn add-melody-event
  [cur-melody-info new-melody-event live-player-id]
  (let [new-melody (assoc (:melody cur-melody-info)
                     (inc (get-last-melody-event-num-for-live-player live-player-id))
                     new-melody-event)]
    (assoc cur-melody-info :melody new-melody)
    )
  )

(defn next-note
  [midi-event live-player-id]
  (swap! (get @live-player-melodies live-player-id)
         add-melody-event
         (create-melody-event live-player-id midi-event)
         live-player-id)
  )

(defn note-end
  [midi-event]
  )

(defn process-note
  [midi-event]
  (println "****************  NOTE RECEIVED ******************")
  (println midi-event)
  (cond (= (:status midi-event) :note-on)
        (if (= (count @live-players) 1)
          (next-note midi-event 0)
          (print-msg "process-note" "******** NOT CURRENTLY SET UP FOR MORE THAN ONE LIVE PLAYER ********"))
        (= (:status midi-event) :note-off)
        (note-end midi-event)
        )
  )

(defn create-live-player
  [player-no]
  (atom {:function process-note
         :live-player-id player-no
         :midi-port (deref (eval (symbol (str "transport.settings/" "midi-port-" player-no))))
         :midi-channel (deref (eval (symbol (str "transport.settings/" "midi-channel-" player-no))))
         :instrument nil
         })
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
  (if (> @number-of-live-players 0)
    (reset! live-players (zipmap
                          (range @number-of-live-players)
                          (map create-live-player (range @number-of-live-players))
                          ))
    )
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
