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
   [transport.ensemble-status :refer [get-ensemble-density-ratio]]
   [transport.players :refer [rand-player-id-excluding-player]]
   [transport.random :refer [weighted-choice]]
   [transport.settings :refer :all]
   [transport.util.constants :refer :all]
   [transport.util.utils :refer [print-msg]]
   )
  (:import transport.behavior.Behavior)
  )

(def behavior-probs (atom [1 1 1 1 1 1]))
(def behavior-probs-len (count @behavior-probs))

(defn- init-behavior-probs
  []
  (swap! behavior-probs #(assoc %1
                           FOLLOW-PLAYER 2
                           CONTRAST-PLAYER 1
                           SIMILAR-PLAYER 2
                           SIMILAR-ENSEMBLE 4
                           CONTRAST-ENSEMBLE 1
                           IGNORE-ALL 1
                           ))
  )

(defn init-behaviors
  []
  (init-behavior-probs)
  )

(defn reset-behaviors
  []
  (init-behaviors))

(defn adjust-single-behavior-prob
  [cur-behavior-probs ndx fnc]
  (assoc cur-behavior-probs
         ndx
         (fnc (get cur-behavior-probs ndx))
         )
  )

(defn adjust-contrast-behavior-probs
  [cur-behavior-probs]
  (let [contrast-player-prob (get @behavior-probs CONTRAST-PLAYER)
        contrast-ensemble-prob (get @behavior-probs CONTRAST-ENSEMBLE)
        ]
    (if (< (get-ensemble-density-ratio) 0.25)
      (cond (and (> contrast-player-prob 0) (> contrast-ensemble-prob 0))
            (-> cur-behavior-probs
                (adjust-single-behavior-prob CONTRAST-PLAYER dec)
                (adjust-single-behavior-prob CONTRAST-ENSEMBLE dec)
                )
            (and (= contrast-player-prob 0) (= contrast-ensemble-prob 0))
            (-> cur-behavior-probs
                (adjust-single-behavior-prob CONTRAST-PLAYER #(if (< 0.7 (rand)) (inc %1) %1))
                (adjust-single-behavior-prob CONTRAST-ENSEMBLE #(if (< 0.7 (rand)) (inc %1) %1))
                )
            (> contrast-player-prob 1)
            (adjust-single-behavior-prob cur-behavior-probs CONTRAST-PLAYER dec)
            (> contrast-ensemble-prob 1)
            (adjust-single-behavior-prob cur-behavior-probs CONTRAST-ENSEMBLE dec)
            :else
            cur-behavior-probs
            )
      (cond (and (> contrast-player-prob 0) (> contrast-ensemble-prob 0))
            cur-behavior-probs
            (and (= contrast-player-prob 0) (= contrast-ensemble-prob 0))
            (-> cur-behavior-probs
                (adjust-single-behavior-prob CONTRAST-PLAYER inc)
                (adjust-single-behavior-prob CONTRAST-ENSEMBLE inc)
                )
            (= contrast-player-prob 0)
            (adjust-single-behavior-prob cur-behavior-probs CONTRAST-PLAYER inc)
            (= contrast-ensemble-prob 0)
            (adjust-single-behavior-prob cur-behavior-probs CONTRAST-ENSEMBLE inc)
            )
      )
    )
  )

(defn- randomize-behavior-probs
  [cur-behavior-probs]
  (print-msg "randomize-behavior-probs" "cur-behavior-probs: " cur-behavior-probs)
  (if (> (rand) 0.9)
    (let [ndx (rand-int behavior-probs-len)]
      (if (> (get cur-behavior-probs ndx) 0)
        (assoc cur-behavior-probs ndx ((if (< (rand) 0.5) inc dec) (get cur-behavior-probs ndx)))
        (assoc cur-behavior-probs ndx (inc (get cur-behavior-probs ndx)))
        )
      )
    cur-behavior-probs)
  )

(defn- get-adjusted-behavior-probs
  []
  (let [new-behavior-probs (-> @behavior-probs
                               (adjust-contrast-behavior-probs)
                               (randomize-behavior-probs)
                               )
        ]
    (if (= 0 (reduce + new-behavior-probs))
      (init-behavior-probs)
      (swap! behavior-probs #(do %2) new-behavior-probs)
      )
    )
  )

(defn select-behavior-action
  [player]
  (weighted-choice (get-adjusted-behavior-probs)) )

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
