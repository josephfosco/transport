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
  (:require
   [overtone.live :refer :all]
   [transport.ensemble :refer [init-ensemble]]
   [transport.ensemble-status :refer [init-ensemble-status reset-ensemble-status]]
   [transport.message-processor :refer [restart-message-processor start-message-processor stop-message-processor]]
   [transport.melody :refer [init-melody reset-melody]]
   [transport.pitch :refer [load-scales]]
   [transport.players :refer [init-players]]
   [transport.schedule :refer [reset-lateness restart-scheduler start-scheduler stop-scheduler]]
   [transport.settings :refer [NUM-PLAYERS set-num-players]]
   [transport.util :refer :all]
   [transport.version :refer :all]
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
   Functions to run transport

     (transport-start)        Start playing
                               optional key :num-players
     (transport-pause)        Pause after playing current notes

   The following functions are not necessary but available:

     (transport-init)         Initialize Transport
                               optional key :num-players
     (transport-restart)      Restart after pausing
                               optional key :num-players
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
      (init-ensemble)
      (reset! is-initialized? true)
      (println "transport successfully initialized"))
    (println "Warning - transport already initialized")))

(declare transport-restart)
(defn transport-start
  "Start playing.

   :num-players - optional key to set the number of players
                  defaults to 10. Retains it's value once set"
  [& {:keys [num-players]
      :or {num-players @NUM-PLAYERS}}]
  (println "transport-start is-playing:" @is-playing?)
  (if (false? @is-playing?)
    (if (true? @restart?)
      (transport-restart :num-players num-players)  ;; already started once - restart instead
      (do
        (println "Starting transport")
        (if (false? @is-initialized?)
          (transport-init :num-players num-players))
        (start-scheduler)
        (start-message-processor)
        (init-melody)
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
  (println "Restarting transport")
  (if (false? @is-playing?)
    (if (true? @restart?)
      (do
        (if (not (nil? num-players))
          (set-num-players num-players))
        (reset-lateness)
        (restart-scheduler)
        (restart-message-processor :reset-listeners true)
        (reset-ensemble-status)    ;; must occur after restart-message-processor
        (init-ensemble)
        (reset! is-playing? true)
        (start-scheduler)
        (start-message-processor)
        ;; if melody reset after scheduler and msg processor won't listen for
        ;; LOUD EVENTmsgs right away
        (reset-melody)
        )
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
  (stop-message-processor)
  (reset! is-playing? false))

(transport-help)
