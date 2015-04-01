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

(ns transport.behaviors
  (:require
   [overtone.live :refer [ranged-rand]]
   [transport.players :refer [rand-player-id-excluding-player]]
   [transport.random :refer [weighted-choice]]
   [transport.settings :refer :all]
   [transport.util.utils :refer [print-msg]]
   )
  (:import transport.behavior.Behavior)
  )

(def behavior-probs (atom [1 1 1 1 1 1]))

(defn init-behaviors
  []
  (reset! behavior-probs (assoc @behavior-probs
                           FOLLOW-PLAYER 2
                           CONTRAST-PLAYER 1
                           SIMILAR-PLAYER 2
                           SIMILAR-ENSEMBLE 4
                           CONTRAST-ENSEMBLE 1
                           IGNORE-ALL 1
                           ))
  )

(defn select-behavior-action
  [player]
  (weighted-choice @behavior-probs) )

(defn select-first-behavior
  "Only used the first time Behavior is set.
   Always sets behavior player-id to nil.

   player - the player to set behavior for"
  [player]
  (let [behavior-action (if (> @number-of-players 1) (select-behavior-action player) IGNORE-ALL)]
    (Behavior. (ranged-rand 0.25 0.85)  ;; accuracy
               behavior-action          ;; action
               nil)                     ;; behavior player-id
    )
  )

(defn select-behavior
  [player]
  (let [behavior-action (if (> @number-of-players 1) (select-behavior-action player) IGNORE-ALL)
        ;; select ensemble-action behavior only if not watching another player
        ]
    (Behavior. (ranged-rand 0.25 0.85)  ;; accuracy
               behavior-action          ;; action
               (if (and (not= behavior-action IGNORE-ALL)
                        (not= behavior-action SIMILAR-ENSEMBLE)
                        (not= behavior-action CONTRAST-ENSEMBLE)
                       )
                 (rand-player-id-excluding-player player) nil)) ;; behavior player-id
    )
  )
