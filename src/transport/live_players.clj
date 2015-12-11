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

  (:use [overtone.live])
  (:require
   [overtone.live :refer [midi->hz]]
   [overtone.midi :as midi]
   [transport.constants :refer [SAVED-MELODY-LEN]]
   [transport.instrument :refer [get-instrument-info-for-name]]
   [transport.instrumentinfo :refer [get-instrument-for-inst-info]]
   [transport.melody.liveplayermelodyevent :refer [create-live-player-melody-event get-live-player-end-time
                                                   get-live-player-note get-live-player-sc-instrument-id
                                                   get-live-player-start-time]]
   [transport.sc-instrument :refer [stop-instrument]]
   [transport.settings :refer [get-setting number-of-live-players]]
   [transport.util.print :refer [print-map print-msg]]
   [transport.util.utils :refer [get-max-map-key]])
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

(defn get-melody-for-live-player-id
  [live-player-id]
  (:melody (deref (get @live-player-melodies live-player-id)))
  )

(defn- get-melody-from-melody-info
  [melody-info]
  (:melody melody-info)
  )

(defn get-last-melody-event-num-for-live-player-id
  [live-player-id]
  (get-max-map-key (get-melody-for-live-player-id live-player-id))
  )

(defn get-last-melody-event-for-live-player-id
  [live-player-id]
  (get (get-melody-for-live-player-id live-player-id)
       (get-max-map-key (get-melody-for-live-player-id live-player-id)))
  )

(defn new-rest-melody-event
  [live-player-id sc-instrument-id start-time end-time]
  (create-live-player-melody-event :note nil
                                   :dur-millis (quot (- end-time start-time) 1000)
                                   :end-time end-time
                                   :start-time start-time
                                   :volume 0
                                   :player-id live-player-id
                                   :sc-instrument-id sc-instrument-id
                                   )
  )

(defn new-melody-event
  [live-player-id midi-event sc-instrument-id]
  (create-live-player-melody-event :note (:note midi-event)
                                   :dur-millis nil
                                   :end-time nil
                                   :start-time (:timestamp midi-event)
                                   :volume (:velocity midi-event)
                                   :player-id live-player-id
                                   :sc-instrument-id sc-instrument-id
                                   )
  )

(defn- adjust-melody-len
  "If necessary, remove the oldest melody-events from cur-melody
   to assure that the melody will not be longer than SAVED-MELODY-LEN
   when num-events-to add are added. Curently only works when num-events-to-add
   = 0, 1 or 2, otherwise throws MoreThan2NewEvents exception

   cur-melody - the melody to remove events from
   num-events-to add - the nimber of events that will be added"
  [cur-melody num-events-to-add next-melody-event-num]
  (let [cur-melody-len (count cur-melody)
        extended-melody-len (+ cur-melody-len num-events-to-add)
        ]
    (cond (= (- extended-melody-len SAVED-MELODY-LEN) 1)
          ;; remove oldest melody-event
          (dissoc cur-melody (- next-melody-event-num SAVED-MELODY-LEN))
          (= (- extended-melody-len SAVED-MELODY-LEN) 2)
          ;; remove 2 oldest melody-event(s)
          (dissoc cur-melody
                  (- next-melody-event-num SAVED-MELODY-LEN)
                  (- next-melody-event-num (dec SAVED-MELODY-LEN))
                  )
          (<= extended-melody-len SAVED-MELODY-LEN)
          ;; use current melody-events as is (there are fewer
          ;; than SAVED-MELODY-LEN events stored
          cur-melody
          :else
          ;; only works with 0, 1 or 2 new events
          (throw (Throwable. "MoreThan2NewEvents")
                 )
          )
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
  (let [next-melody-event-num (inc (get-last-melody-event-num-for-live-player-id live-player-id))
        prior-end-time (get-live-player-end-time (get-last-melody-event-for-live-player-id live-player-id))
        cur-start-time (get-live-player-start-time new-melody-event)
        rest-event (if (and prior-end-time (> (- cur-start-time prior-end-time) 100000))
                     (new-rest-melody-event live-player-id
                                            (get-live-player-sc-instrument-id new-melody-event)
                                            (inc prior-end-time)
                                            (dec cur-start-time)
                                            )
                     nil
                     )
        new-melody (if rest-event
                     (assoc (adjust-melody-len (get-melody-from-melody-info cur-melody-info)
                                               2
                                               next-melody-event-num)
                       next-melody-event-num rest-event
                       (inc next-melody-event-num) new-melody-event
                       )
                     (assoc (adjust-melody-len (get-melody-from-melody-info cur-melody-info)
                                               1
                                               next-melody-event-num)
                       next-melody-event-num new-melody-event
                       )
                     )
        ]
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
           )
    )
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
  (let [last-melody-event-num (get-last-melody-event-num-for-live-player-id live-player-id)]
    (loop [melody-event-num last-melody-event-num]
      (when (< melody-event-num 1)
        (throw (Throwable. "NoNoteToStop"))
        )
      (let [melody-event (get (get-melody-from-melody-info cur-melody-info) melody-event-num)]
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
        new-melody-event (assoc melody-event :dur-millis dur :end-time (:timestamp midi-event))
        ]
    (stop-instrument (get-live-player-sc-instrument-id melody-event))
    (assoc cur-melody-info :melody (assoc (get-melody-from-melody-info cur-melody-info)
                                     melody-event-num new-melody-event))
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
  (print-msg "process-note "
             "****************  NOTE RECEIVED: "
             (:status midi-event) " " (:note midi-event)
             " ***************")
    (let [live-player-id (get-player-for-midi-event midi-event)]
      (case (:status midi-event)
        :note-on (next-note midi-event live-player-id)
        :note-off (note-end midi-event live-player-id)
        )
      )
  )

(defn set-up-midi
  [live-player-id midi-transmitter fnc]
  {live-player-id {:midi-receiver (midi/midi-handle-events midi-transmitter fnc)}}
  )

(defn- change-player-fnc
  "This function is only used after the first midi event is received
   to have the player use the process-note function for all
   subsequent midi events"
  [cur-live-player]
  (let [l-player  (merge cur-live-player {:function process-note})
        midi-receiver (set-up-midi
                       (:live-player-id l-player)
                       (:midi-transmitter l-player)
                       (:function l-player)
                       )]
    (merge l-player (first (vals midi-receiver)))
    )
  )

(defn first-melody-event
  "Add first melody event to live-player

   cur-melody-info - current melody info for live-player
   live-player-id - id of live-player that is haveing
                    it's melody updated
   new-melody-event - the melody event to add"
  [midi-event]
  (print-msg "first-melody-event"
             " ****************  NOTE RECEIVED: "
             (:status midi-event) " " (:note midi-event)
             " *****************")
  (let [live-player-id (get-player-for-midi-event midi-event)]
    (if (= (:melody (deref (get @live-player-melodies live-player-id))) {})
      (case (:status midi-event)
        :note-on (do
                   (next-note midi-event live-player-id)
                   (let [rcvr (:midi-receiver (deref (get @live-players live-player-id)))]
                     (swap! (get @live-players live-player-id) change-player-fnc)
                     )
                   )
        :note-off (do
                    (print-msg "first melody-event" " ERROR note-off in first-melody-event ERROR")
                    (print-msg "first melody-event" " ERROR live-player-id: " live-player-id)
                    (print-msg "first melody-event" " ERROR midi-event: " midi-event)
                    )
        )
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
        midi-port (deref (eval (symbol (str "transport.settings/" "midi-port-" player-no))))
        ]

    (atom {:function first-melody-event
           :instrument-info instrument-info
           :live-player-id player-no
           :midi-port midi-port
           :midi-channel (deref (eval (symbol (str "transport.settings/" "midi-channel-" player-no))))
           :midi-receiver nil
           :midi-transmitter (midi/midi-in midi-port)
           })
    )
  )

(defn initial-live-player-melody
  [live-player-no]
  {
   :live-player-id live-player-no
   :melody {}
   }
  )

(defn- reset-live-player
  [live-player-id new-live-player-info]
  (reset! (get @live-players live-player-id) new-live-player-info)
  )

(defn init-live-players
  [& {:keys [init-midi-ports] :or {init-midi-ports true}}]
  (when (> @number-of-live-players 0)
    (reset! live-players (zipmap
                          (range @number-of-live-players)
                          (map create-live-player (range @number-of-live-players))
                          ))
    ;; Map midi-port(s) to midi function and add  to live-player map
    (when init-midi-ports
      (let [players (map deref (vals @live-players))
            midi-receivers (map set-up-midi
                                (map :live-player-id players)
                                (map :midi-transmitter players)
                                (map :function players))
            ]
        (doall (for [rcvr midi-receivers lp-id (keys @live-players)
                     :when (= (first (keys rcvr)) lp-id)
                     ]
                 (reset-live-player lp-id (merge (deref (get @live-players lp-id)) (first (vals rcvr))))
                 ))
        )
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

(defn print-melody
  [melody]
  (print-map melody)
 )

(defn print-melody-for-live-player-id
  [live-player-id]
  (print-melody (:melody (deref (get @live-player-melodies live-player-id))))
  )
