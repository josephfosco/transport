;    Copyright (C) 2013-2015 Joseph Fosco. All Rights Reserved
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
   [transport.control :refer [clear-transport pause-transport quit-transport
                              start-transport ]]
   [transport.version :refer :all]
   ))

 (defn -main
   [& args]
   (println "command line args:" args)
  )

(defn transport-start
  "Start playing.

   :num-players - optional key to set the number of players
                  default value is set in config file"
  [& {:keys [num-players]}]
  (start-transport :num-players num-players)
)

(defn transport-quit
  "Quit Transport and exit Clojure"
  []
  (quit-transport)
  )

(defn transport-exit
  "same as transport-quit"
  []
  (quit-transport))

(defn transport-pause
  "Stop playing after players finish what they have scheduled"
  []
  (pause-transport)
)

(defn transport-clear
  "Clears the scheduler, message-processor, and players"
  []
  (clear-transport)
  )

(defn transport-help
  []
  (println)
  (println "TRANSPORT version" TRANSPORT-VERSION-STR)
  (print
   "
   Functions to run transport

     (transport-start)        Start playing
                               optional key :num-players
     (transport-pause)        Pause after playing current notes

     (transport-help)         Print this message


"))

(defn start64
  []
  (transport-start :num-players 64))

(defn stop
  []
  (overtone.live/stop))

(transport-help)
