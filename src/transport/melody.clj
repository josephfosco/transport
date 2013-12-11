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
   [transport.behavior :only [get-behavior-action FOLLOW]]
   [transport.pitch :only [next-pitch]]
   [transport.players :only [get-behavior-player-id get-melody get-player get-player-id]]
   [transport.random :only [random-int]]
   [transport.rhythm :only [next-note-dur]]))

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

(defn get-last-melody-event-num
  [player-id]
  (let [last-melody-key (reduce max 0 (keys (get-melody (get-player player-id))))]
    (if (= last-melody-key 0) nil last-melody-key)
    )
  )

(defn get-melody-event
  [player-id melody-event-no]
  (get (get-melody (get-player player-id)) melody-event-no)
  )

(defn next-melody-follow
  [player]
  (let [follow-player-id (get-behavior-player-id player)
        last-note-played (get-last-melody-event-num (get-player-id player))
        cur-note-to-play (if (= last-note-played nil)
                           (get-last-melody-event-num follow-player-id)
                           (+ last-note-played 1))
        next-melody-event (get-melody-event follow-player-id cur-note-to-play)
        ]
    (println)
    (println " PLAYER-ID:" (get-player-id player))
    (println "player :malody " (get-melody player))
    (println "last-note-played: " last-note-played)
    (println "next-melody-event: " next-melody-event)
    ;; if the :follow-player has not played a note yet (cur-note-to-play = nil
    ;; pick a note to plat else
    ;; play the next follow player note
    (if (= next-melody-event nil)
      {:note (next-pitch player)
       :dur-info (next-note-dur player)
       :follow-note last-note-played}
      (if (not= cur-note-to-play nil)
        (do
          (println (assoc next-melody-event :follow-note cur-note-to-play))
          (assoc (get-melody-event follow-player-id cur-note-to-play) :follow-note cur-note-to-play)
          )
        {:note (next-pitch player)
         :dur-info (next-note-dur player)})))
  )

(defn next-melody
  "Returns the next note and it's duration as a map
    containing the keys :note and :dur-info

    player is the player map"
  [player]
  (if (= (get-behavior-action player) FOLLOW)
    (next-melody-follow player)
    {:note (if (note-or-rest 8) (next-pitch player) nil)
     :dur-info (next-note-dur player) })
  )
