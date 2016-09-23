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
;; Update the player rules
;;

(ns transport.rules.next-note-rules
  (:require
   [polyphony.core :refer :all]
   [transport.constants :refer :all]
   [transport.external-function-api]
   )
  )

(defrule
  (
   (= ?player-updated false)
   (= ?needs-new-segment true)
   )
  (
   (transport.external-function-api/update-segment-for-player)
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
   (transport.external-function-api/update-follow-info-for-player)
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
   (transport.external-function-api/update-ensemble-info-for-player)
   (set-var ?player-updated true)
   )
  )

(defrule
  (
   (= ?player-updated false)
   (= ?needs-new-segment false)
   (= ?similar-ensemble false)
   )
  (
   (set-var ?player-updated true)
   )
  )

;;
;;
;;
