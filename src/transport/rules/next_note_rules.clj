;    Copyright (C) 2016  Joseph Fosco. All Rights Reserved
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

;;
;; Rules for updating the player
;;

(ns transport.rules.next-note-rules
  (:require
   [polyphony.core :refer :all]
   [transport.constants :refer :all]
   [transport.play-note :as tpn]
   )
  )

(defrule
  (
   (= ?player-updated false)
   (= ?needs-new-segment true)
   )
  (
   (tpn/update-segment-for-player)
   (set-var ?player-updated true)
   )
  )

(defrule
  (
   (= ?player-updated false)
   (= ?needs-new-segment false)
   (= ?new-follow-info true)
   )
  (
   (tpn/update-follow-info-for-player)
   (set-var ?player-updated true)
   )
  )

(defrule
  (
   (= ?player-updated false)
   (= ?needs-new-segment false)
   (= ?behavior-action SIMILAR-ENSEMBLE)
   )
  (
   (tpn/update-ensemble-info-for-player)
   (set-var ?player-updated true)
   )
  )

(defrule
  (
   (= ?player-updated false)
   (= ?needs-new-segment true)
   (= ?behavior-action SIMILAR-ENSEMBLE)
   )
  (
   (set-var ?player-updated true)
   )
  )

(defrule
  (
   (= ?player-updated false)
   (= ?needs-new-segment false)
   (not= ?behavior-action SIMILAR-ENSEMBLE)
   )
  (
   (set-var ?player-updated true)
   )
  )

;;
;; Once the player has been updated,
;; get and play next melody event
;;

(defrule
  (
   (= ?create-melody-event true)
   (= ?behavior-action FOLLOW-PLAYER)
   (= ?needs-new-segment true)
   )
  (
   (tpn/sync-player-play-melody)
   )
  )

(defrule
  (
   (= ?create-melody-event true)
   (= ?behavior-action FOLLOW-PLAYER)
   (= ?needs-new-segment false)
   )
  (
   (tpn/follow-play-melody)
   )
  )

(defrule
  (
   (= ?create-melody-event true)
   (= ?behavior-action SIMILAR-ENSEMBLE)
   (= ?needs-new-segment true)
   )
  (
   (tpn/sync-ensemble-play-melody)
   )
  )

(defrule
  (
   (= ?create-melody-event true)
   (= ?behavior-action SIMILAR-ENSEMBLE)
   (= ?needs-new-segment false)
   )
  (
   (tpn/no-sync-play-melody)
   )
  )

(defrule
  (
   (= ?create-melody-event true)
   (not= ?behavior-action FOLLOW-PLAYER)
   (not= ?behavior-action SIMILAR-ENSEMBLE)
   )
  (
   (tpn/no-sync-play-melody)
   )
  )
