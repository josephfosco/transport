;    Copyright (C) 2013-2016 Joseph Fosco. All Rights Reserved
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

(ns transport.control
  (:require
   [overtone.live :refer :all]
   [transport.rules.next_note_rules]
   [transport.ensemble-status :refer [init-ensemble-status reset-ensemble-status]]
   [transport.behaviors :refer [init-behaviors reset-behaviors]]
   [transport.live-players :refer [init-live-players]]
   [transport.message-processor :refer [clear-message-processor restart-message-processor start-message-processor
                                        stop-message-processor]]
   [transport.melody :refer [init-melody reset-melody]]
   [transport.play-note :refer [init-ensemble reset-ensemble]]
   [transport.pitch :refer [load-scales]]
   [transport.schedule :refer [clear-scheduler init-lateness reset-scheduler
                               restart-scheduler start-scheduler stop-scheduler]]
   [transport.settings :refer [reset-setting set-number-of-players]]
   [transport.util.log :as log]
   [transport.util.print :refer [print-banner]]
   [transport.util.utils :refer :all]
   [transport.version :refer :all]
   ))

(def ^:private is-initialized? (atom false))
(def ^:private is-playing? (atom false))
(def ^:private restart? (atom false))  ;; set to true once playuing has started - remains true after that

(defn- init-transport
  "Initialize transport to play. Use only once (first time)

   keyword args -
   :num-players - optional, the number of players playing.
                  default value is set in config file"
  [& {:keys [num-players]}]
  (if (false? @is-initialized?)
    (do
      (if num-players (set-number-of-players num-players))
      (transport.pitch/load-scales)
      (transport.behaviors/init-behaviors)
      (log/data2 (with-out-str (print-banner "init-transport about to init-lateness in schedule")))
      (init-lateness)

      (log/data2 (with-out-str (print-banner "init-transport about to init-ensemble")))
      (init-ensemble)

      (log/data2 (with-out-str (print-banner "init-transport about to init-live-players")))
      (init-live-players)

      (log/data2 (with-out-str (print-banner "init-transport about to init-ensemble-status")))
      (init-ensemble-status)

      (log/data2 (with-out-str (print-banner "init-transport about to init-melody")))
      (init-melody)
      (log/data2 (with-out-str (print-banner "init-transport init-melody-complete")))

      (reset! is-initialized? true)

      (log/data2 (with-out-str (print-banner "init-transport about to add output reverb")))
      (def out-rvrb (fx-freeverb :wet-dry 0.3 :room-size 0.3 :dampening 0.4))

      (log/data2 (with-out-str (print-banner "transport successfully initialized")))
      )
    (log/warn (with-out-str (print-banner "Warning - transport already initialized" :prefix "!!!")))
    ))

(declare restart-transport)
(defn start-transport
  "Start playing.

   :num-players - optional key to set the number of players
                  default value is set in config file"
  [& {:keys [num-players]}]

  (log/data2 (log/format-msg "start-transport" "is-playing: " @is-playing?))
  (log/data2 (log/format-msg "start-transport" "restart: " @restart?))
  (if (false? @is-playing?)
    (if (true? @restart?)
      ;; already started once - restart instead
      (restart-transport :num-players num-players)
      (do
        (log/info (with-out-str (print-banner "Starting transport")))
        (if (false? @is-initialized?)
          (init-transport :num-players num-players))

        (log/data2 (with-out-str (print-banner "start-transport about to start-message-processor")))
        (start-message-processor)

        (log/data2 (with-out-str (print-banner "start-transport about to start-scheduler")))
        (start-scheduler)

        (reset! is-playing? true)
        (reset! restart? true)

        (log/data2 (with-out-str (print-banner (str "start-transport - restart:" @restart?))))

        (transport.schedule/reset-lateness)

        (log/info (with-out-str (print-banner "start-transport - start complete")))
        ))
    (log/warn (with-out-str (print-banner "start-transport - WARNING - Can't start. Already Playing." :prefix "!!!")))))

(defn- restart-transport
  "Start transport after pausing.
   Restarts scheduler, Initializes players, starts playing

   keyword args -
   :num-players - optional, the number of players playing.
                  defaults to it's prior value"
  [& {:keys [num-players]}]
  (log/info (with-out-str (print-banner "Restarting transport")))
  (if (false? @is-playing?)
    (if (true? @restart?)
      (do
        (if num-players (set-number-of-players num-players))
        (reset-behaviors)
        (reset-scheduler)
        (restart-scheduler)
        (restart-message-processor :reset-listeners true)
        (init-ensemble)
        (init-live-players :init-midi-ports false)
        (log/data2 (with-out-str (print-banner "restart-transport about to reset-ensemble-status")))
        (reset-ensemble-status)

        (reset! is-playing? true)

        (log/data2 (with-out-str (print-banner "restart-transport about to start-message-processor")))
        (start-message-processor)

        (log/data2 (with-out-str (print-banner "restart-transport about to start-scheduler")))
        (start-scheduler)

        (log/data2 (with-out-str (print-banner "restart-transport about to reset-melody")))
        ;; if melody reset after scheduler and msg processor won't listen for
        ;; LOUD EVENT msgs right away
        (reset-melody)

        (transport.schedule/reset-lateness)

        (log/info (with-out-str (print-banner "restart-transport restart complete")))
        )
      (start-transport))
    (log/warn (with-out-str (print-banner "restart-transport WARNING - Can't restart. Already playing" :prefix "!!!")))))

(defn pause-transport
  "Stop playing after players finish what they have scheduled"
  []
  (stop-scheduler)
  (stop-message-processor)
  (reset! is-playing? false))

(defn quit-transport
  "Quit Transport and exit Clojure"
  []
  (transport.schedule/shutdown-event-queue)
  )

(defn clear-transport
  "Clears the scheduler, message-processor, and players"
  []
  (clear-scheduler)
  (clear-message-processor)
  (reset-ensemble)
  )
