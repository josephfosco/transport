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

(ns transport.core
  (:gen-class)
  (:use
   [overtone.live]
   [transport.ensemble :only [init-players]]
   [transport.pitch :only [load-scales]]
   [transport.schedule :only [restart-scheduler start-scheduler stop-scheduler]]
   ))

 (defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn transport-help []
  (print
   "

   Avilable functions for transport

   (transport-init)         Initialize Transport
   (transport-start)        Start playing
   (transport-pause)        Pause after playing current notes
   (transport-restart)      Restart after pausing - requires new init
   (transport-init-players) Initialize players - after pausing


"))

(defn transport-init []
  (transport.pitch/load-scales)
  (init-players))

(defn transport-quit
  "Quit Transport and exit Clojure"
  []
  (transport.schedule/shutdown-event-queue)
  )

(defn transport-exit
  "same as transport-quit"
  []
  (transport-quit))

(defn transport-pause
  "Stop playing after players finish what they have scheduled
   To restart after pause, players must be initialized"
  []
  (stop-scheduler))

(defn transport-restart
  "Start transport after pausing."
  []
  (restart-scheduler))

(defn transport-init-players []
  (init-players))

(defn transport-start
  "Start playing. Use after initializing players wih
   (transport-init) or (transport-init-players)"
  []
  (start-scheduler))

(transport-help)
