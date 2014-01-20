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

(ns transport.util
  (:use
   [transport.players :only [get-melody get-players]]
   ))

(defn print-player
  "Pretty Print a player map

  player - the player map to print"
  [player]
  (println "player:")
  (println "  :behavior         " (:behavior player))
  (println "  :cur-note-beat    " (:cur-note-beat player))
  (println "  :function         " (:function player))
  (println "  :instrument :name " (:name (:instrument (:instrument-info player))))
  (println "  :key              " (:key player))
  (println "  :melody           " (get-melody player))
  (println "  :mm               " (:mm player))
  (println "  :player-id        " (:player-id player))
  (println "  :prev-note-beat   " (:prev-note-beat player))
  (println "  :range-lo         " (:range-lo (:instrument-info player)))
  (println "  :range-hi         " (:range-hi (:instrument-info player)))
  (println "  :seg-len          " (:seg-len player))
  (println "  :seg-start        " (:seg-start player))
  (println "  :scale            " (:scale player))
)
(defn print-player-long
  "Pretty Print a player map with all instrument-info

  player - the player map to print"
  [player]
  (print-player)
  (println "  :instrument-info  " (:instrument-info player))
)

(defn print-all-players
  []
  (dorun (map print-player (get-players)))
)
