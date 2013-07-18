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
  (println "")
  (println "")
  (println "Avilable functions for transport")
  (println "(init-transport) (init-players)")
  (println "(start-transport)")
  (println "(stop-scheduler) (restart-scheduler)")
  (println "")
  (println ""))

(defn init-transport []
  (transport.pitch/load-scales)
  (init-players))

(defn start-transport []
  (start-scheduler))

(transport-help)
