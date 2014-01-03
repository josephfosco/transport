;    Copyright (C) 2013-2014  Joseph Fosco. All Rights Reserved
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
   [transport.ensemble-status :only [init-ensemble-status reset-ensemble-status]]
   [transport.pitch :only [load-scales]]
   [transport.schedule :only [reset-lateness restart-scheduler start-scheduler stop-scheduler]]
   [transport.settings :only [set-num-players]]
   [transport.util]
   [transport.version]
   ))

 (defn -main
  [& args]
  )

(def is-initialized? (atom false))
(def is-playing? (atom false))
(def restart? (atom false))  ;; set to true once playuing has started - remains true after that

(defn transport-help
  []
  (print
   "
   ")
  (println "TRANSPORT version" TRANSPORT-VERSION-STR)
  (print
   "
   Avilable functions for transport

   (transport-init)         Initialize Transport
   (transport-start)        Start playing
   (transport-pause)        Pause after playing current notes
   (transport-restart)      Restart after pausing
   (transport-help)         Print this message


"))

(defn transport-init
  "Initialize transport to play. Use only once (first time)

   keyword args -
   :num-players - optional, the number of players playing.
                  default value is 10"
  [& {:keys [num-players]
      :or {num-players 10}}]
  (if (false? @is-initialized?)
    (do
      (set-num-players num-players)
      (transport.pitch/load-scales)
      (init-ensemble-status)
      (init-players)
      (reset! is-initialized? true)
      (println "transport successfully initialized"))
    (println "Warning - transport already initialized")))

(declare transport-restart)
(defn transport-start
  "Start playing. Use after initializing players wih
   (transport-init) or (transport-init-players)"
  []
  (if (false? @is-playing?)
    (if (true? @restart?)
      (transport-restart)  ;; already started once - restart instead
      (do
        (if (false? @is-initialized?)
          (transport-init))
        (start-scheduler)
        (reset! is-playing? true)
        (reset! restart? true)
        ))
    (println "WARNING - Can't start. Already Playing.")))

(defn transport-restart
  "Start transport after pausing.
   Restarts scheduler, Initializes players, starts playing

   keyword args -
   :num-players - optional, the number of players playing.
                  defaults to it's prior value"
  [& {:keys [num-players]
      :or {num-players nil}}]
  (if (false? @is-playing?)
    (if (true? @restart?)
      (do
        (if (not (nil? num-players))
          (set-num-players num-players))
        (reset-ensemble-status)
        (reset-lateness)
        (restart-scheduler)
        (init-players)
        (reset! is-playing? true)
        (start-scheduler))
      (transport-start))
    (println "WARNING - Can't restart. Already playing")))

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
  "Stop playing after players finish what they have scheduled"
  []
  (stop-scheduler)
  (reset! is-playing? false))

(transport-help)
