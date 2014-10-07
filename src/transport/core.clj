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
   [transport.message-processor :refer [clear-message-processor restart-message-processor start-message-processor stop-message-processor]]
   [transport.melody :refer [init-melody reset-melody]]
   [transport.pitch :refer [load-scales]]
   [transport.players :refer [reset-players init-players]]
   [transport.schedule :refer [clear-scheduler reset-scheduler restart-scheduler start-scheduler stop-scheduler]]
   [transport.settings :refer [number-of-players set-number-of-players]]
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
      (set-number-of-players num-players)
      (transport.pitch/load-scales)
      (init-ensemble-status)
      (init-ensemble)

      (print-banner "transport-init about to init-melody")
      (init-melody)
      (reset! is-initialized? true)

      (print-banner "transport successfully initialized")
      )
    (print-banner "Warning - transport already initialized" :prefix "!!!")
    ))

(declare transport-restart)
(defn transport-start
  "Start playing.

   :num-players - optional key to set the number of players
                  defaults to 10. Retains it's value once set"
  [& {:keys [num-players]
      :or {num-players @number-of-players}}]
  (print-msg "transport-start" "is-playing: " @is-playing?)
  (print-msg "transport-start" "restart: " @restart?)
  (if (false? @is-playing?)
    (if (true? @restart?)
      (transport-restart :num-players num-players)  ;; already started once - restart instead
      (do
        (print-banner "Starting transport")
        (if (false? @is-initialized?)
          (transport-init :num-players num-players))

        (print-banner "transport-start about to start-scheduler")
        (start-scheduler)

        (print-banner "transport-start about to start-message-processor")
        (start-message-processor)

        (print-banner "transport-start init-melody-complete")
        (reset! is-playing? true)
        (reset! restart? true)

        (print-banner (str "transport-start - restart:" @restart?))
        (print-banner "transport-start - start complete")
        ))
    (print-banner "transport-start - WARNING - Can't start. Already Playing." :prefix "!!!")))

(defn transport-restart
  "Start transport after pausing.
   Restarts scheduler, Initializes players, starts playing

   keyword args -
   :num-players - optional, the number of players playing.
                  defaults to it's prior value"
  [& {:keys [num-players]
      :or {num-players nil}}]
  (print-banner "Restarting transport")
  (if (false? @is-playing?)
    (if (true? @restart?)
      (do
        (if (not (nil? num-players))
          (set-number-of-players num-players))
        (reset-scheduler)
        (restart-scheduler)
        (restart-message-processor :reset-listeners true)
        (reset-ensemble-status)    ;; must occur after restart-message-processor
        (init-ensemble)
        (reset! is-playing? true)

        (print-banner "transport-restart about to start-scheduler")
        (start-scheduler)

        (print-banner "transport-restart about to start-message-processor")
        (start-message-processor)

        (print-banner "transport-restart about to reset-melody")
        ;; if melody reset after scheduler and msg processor won't listen for
        ;; LOUD EVENTmsgs right away
        (reset-melody)

        (print-banner "transport-restart restart complete")
        )
      (transport-start))
    (print-banner "transport-restart WARNING - Can't restart. Already playing" :prefix "!!!")))

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

(defn transport-clear
  "Clears the scheduler, message-processor, and players"
  []
  (clear-scheduler)
  (clear-message-processor)
  (reset-players)
  )

(transport-help)
