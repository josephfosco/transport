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
   [transport.constants :refer :all]
   [transport.ensemble-status :refer [get-ensemble-density-ratio]]
   [transport.players :refer [rand-player-id-excluding-player]]
   [transport.random :refer [weighted-choice]]
   [transport.settings :refer :all]
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
  ([cur-behavior-probs ndx fnc]
     (assoc cur-behavior-probs
       ndx
       (fnc (get cur-behavior-probs ndx))
       )
     )
  ([cur-behavior-probs ndx fnc x]
     (assoc cur-behavior-probs
       ndx
       (fnc (get cur-behavior-probs ndx) x)
       )
     )
  )

(defn adjust-val-randomly
  [val threshold]
  (if (< (rand) threshold) (inc val) val)
  )

(defn adjust-contrast-probs
  [cur-behavior-probs contrast-player-prob contrast-ensemble-prob]
  (let [prob-threshold (/ (reduce + cur-behavior-probs) 12)]
    (cond (or (and (>= contrast-player-prob prob-threshold) (>= contrast-ensemble-prob prob-threshold))
              (and (> (rand) 0.5) (> contrast-player-prob 0) (> contrast-ensemble-prob 0))
              )
          (-> cur-behavior-probs
              (adjust-single-behavior-prob CONTRAST-PLAYER dec)
              (adjust-single-behavior-prob CONTRAST-ENSEMBLE dec)
              )
          (and (< contrast-player-prob prob-threshold) (< contrast-ensemble-prob prob-threshold))
          (-> cur-behavior-probs
              (adjust-single-behavior-prob CONTRAST-PLAYER adjust-val-randomly 0.4)
              (adjust-single-behavior-prob CONTRAST-ENSEMBLE adjust-val-randomly 0.4)
              )
          (> contrast-player-prob prob-threshold)
          (adjust-single-behavior-prob cur-behavior-probs CONTRAST-PLAYER dec)
          (> contrast-ensemble-prob prob-threshold)
          (adjust-single-behavior-prob cur-behavior-probs CONTRAST-ENSEMBLE dec)
          :else
          cur-behavior-probs
          )
    )
  )

(defn adjust-contrast-behavior-probs
  [cur-behavior-probs]
  (let [contrast-player-prob (get cur-behavior-probs CONTRAST-PLAYER)
        contrast-ensemble-prob (get cur-behavior-probs CONTRAST-ENSEMBLE)
        ]
    (if (or (< (get-ensemble-density-ratio) 0.25) (> (get-ensemble-density-ratio) 0.75))
      ;; possibly increment SIMILAr-ENSEMBLE as long as it's current value is not > 1/2 all values
      (-> (if (>= (get cur-behavior-probs SIMILAR-ENSEMBLE) (/ (reduce + cur-behavior-probs) 2))
            cur-behavior-probs
            (adjust-single-behavior-prob cur-behavior-probs
                                         SIMILAR-ENSEMBLE
                                         adjust-val-randomly
                                         (/ 1 @number-of-players))
            )
          (adjust-contrast-probs contrast-player-prob contrast-ensemble-prob)
          )
      cur-behavior-probs
      )
    )
  )

(defn- randomize-behavior-probs
  [cur-behavior-probs]
  ;;(print-msg "randomize-behavior-probs" "cur-behavior-probs:    " cur-behavior-probs)
  (if (< (rand-int @number-of-players) (* 0.05 @number-of-players))
    (let [ndx (rand-int behavior-probs-len)
          behavior-prob (get cur-behavior-probs ndx)
          ]
      (cond (and (> behavior-prob 0)
                 (or (and (not= ndx SIMILAR-ENSEMBLE)
                          (<= behavior-prob (/ (reduce + cur-behavior-probs) 3))
                          )
                     (and (= ndx SIMILAR-ENSEMBLE)
                          (<= behavior-prob (/ (reduce + cur-behavior-probs) 2))
                          )
                     )
                 )
            (assoc cur-behavior-probs ndx ((if (< (rand) 0.5) inc dec) behavior-prob))
            (= behavior-prob 0)
            (assoc cur-behavior-probs ndx (inc behavior-prob))
            :else
            (assoc cur-behavior-probs ndx (dec behavior-prob))
          )
        )
      cur-behavior-probs)
  )

(defn- get-adjusted-behavior-probs
  [cur-behavior-probs]
  (let [new-behavior-probs (-> cur-behavior-probs
                               (adjust-contrast-behavior-probs)
                               (randomize-behavior-probs)
                               )
        ]
    (print-msg "get-adjusted-behavior-probs" "new-behavior-probs: " new-behavior-probs)
    (if (= 0 (reduce + new-behavior-probs))
      (init-behavior-probs)
      new-behavior-probs
      )
    )
  )

(defn select-behavior-action
  [player]
  (weighted-choice (swap! behavior-probs get-adjusted-behavior-probs)) )

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
