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

(ns transport.rules.next-note-rules
  (:require
   [polyphony.core :refer :all]
   ;;[transport.play-note]
   )
  )

(comment
  (defrule
    (
     (= ?player-updated false)
     (= ?needs-new-segment true)
     )
    (
     (transport.play-note/update-player-segment)
     (set-var ?player-updated true)
     )
    )
  )

(defrule
  (
   (= ?player-updated false)
   (= ?needs-new-segment true)
   )
  (
   (println "****************** NEW SEGMENT ********************")
   (set-var ?player-updated true)
   )
  )

(defrule
  (
   (= ?new-follow-info 1)
   )
  (())
  )

(comment
  (defrule
    ((not= ?player-id nil)
     (not= ?event-time nil)
     (not= ?needs-new-segment nil)
     (not= ?new-follow-info nil))
    (
     (transport.play-note/update-and-swap-player
      ?player-id
      ?event-time
      ?needs-new-segment
      ?new-follow-info)
     )
    )
  )
