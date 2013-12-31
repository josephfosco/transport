;    Copyright (C) 2013  Joseph Fosco. All Rights Reserved
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
  (:use
   [transport.behavior :only [get-behavior-action get-behavior-ensemble-action]]
   [transport.pitch :only [next-pitch]]
   [transport.ensemble-status :only [get-rest-probability]]
   [transport.players :only [get-behavior-player-id get-dur-info get-last-melody-event get-melody get-note get-player get-player-id]]
   [transport.random :only [random-int]]
   [transport.rhythm :only [get-dur-info-for-beats next-note-dur]]
   [transport.settings :only [COMPLEMENT FOLLOW]]
   ))

(defn note-or-rest
  "Determines what note to play next.
   If player is supposed to rest, returns nil

   note-prob is the probability of a note as opposed to a rest
   note-prob of 8 means 8 times out of 9 a note will play
   +note-prob of 5 means 5 times out of 6 a note will play"
  [note-prob]
  (let [play-note? (random-int 0 note-prob)]
    (if (pos? play-note?)
      true
      nil)))

(defn note-or-rest-follow-ensemble
  [player]
  (if (< (rand) (get-rest-probability)) nil true))

(defn get-last-melody-event-num
  [player-id]
  (let [last-melody-key (reduce max 0 (keys (get-melody (get-player player-id))))]
    (if (= last-melody-key 0) nil last-melody-key)
    )
  )

(defn get-volume
  [melody-event]
  (:volume melody-event))

(defn get-melody-event
  [player-id melody-event-no]
  (get (get-melody (get-player player-id)) melody-event-no))

(defn next-melody-follow
  [player]
  (let [follow-player-id (get-behavior-player-id player)
        follow-player-last-note (get-last-melody-event-num follow-player-id)
        next-new-event {:note nil
         :dur-info (get-dur-info-for-beats (get-player follow-player-id) 3)
         :follow-note (if (nil? follow-player-last-note)
                        0
                        (- follow-player-last-note 1))}
        ]
    (if (nil? (:follow-note (get-last-melody-event player)))
      ;; first time, rest 3 beats
      next-new-event
      (let [
            last-melody-event-played (get-last-melody-event player)
            cur-note-to-play (+ (:follow-note last-melody-event-played) 1)
            next-melody-event (get-melody-event follow-player-id cur-note-to-play)
            ]
        (if (nil? next-melody-event)
          ;; FOLLOWer ahead of FOLLOWed
          (assoc last-melody-event-played :follow-note (:follow-note last-melody-event-played))
          (assoc next-melody-event :follow-note cur-note-to-play))
        )))
  )

(defn next-melody-follow-ensemble
  [player]
  (let [next-note-or-rest (if (note-or-rest-follow-ensemble player) (next-pitch player) nil)]
    {:note next-note-or-rest
     :dur-info (next-note-dur player)
     :volume 0.01 }
    ))

(defn next-melody
  "Returns the next note and it's duration as a map
    containing the keys :note and :dur-info

    player is the player map"
  [player]
  (cond
   (= (get-behavior-action player) FOLLOW) (next-melody-follow player)
   (= (get-behavior-ensemble-action player) COMPLEMENT) (next-melody-follow-ensemble player)
   :else {:note (if (note-or-rest 8) (next-pitch player) nil)
          :dur-info (next-note-dur player)
          :volume 1.0 })
  )
