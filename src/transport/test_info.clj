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

(ns transport.test-info
  (:require
   [transport.behavior :refer :all]
   [transport.players :refer :all]
   [transport.settings :refer :all]
   )
  (:import transport.behavior.Behavior)
  )

(def tst-player {:key 3 :scale :hex-major6 :instrument-info {:range-hi 90 :range-lo 62} :melody [{:note 70} { :note 72} {:note 74}] :melody-char {:smoothness 0} :mm 60  :behavior (Behavior. 0.5 FOLLOW IGNORE 2)})

(defn lstnr [& {:keys [old new]}] (println "lstnr: " old new))

(comment
  {
   :behavior #transport.behavior.Behavior{:accuracy 0.2966165302092916, :action 3, :ensemble-action 0, :player-id 3}
   :cur-note-beat 0
   :function  #<ensemble$play_melody transport.ensemble$play_melody@75a80485>
   :instrument-info :name saw-wave-sus
   :instrument-info :range-lo 82
   :instrument-info :range-hi 113
   :key 0
   :melody {}
   :melody-char {:continuity 9, :density 4, :range 2, :smoothness 0}
   :metronome #<Metronome overtone.music.rhythm.Metronome@7ada4eda>
   :mm 64
   :player-id 2
   :prev-note-beat 0
   :scale :ionian
   :seg-len 19726
   :seg-start 0
   }

  {
   :behavior #transport.behavior.Behavior{:accuracy 0.4798252752058973, :action 3, :ensemble-action 0, :player-id 4}
   :cur-note-beat 0
   :function #<ensemble$play_melody transport.ensemble$play_melody@75a80485>
   :instrument-info :name tri-wave-sus
   :instrument-info :range-lo 82
   :instrument-info :range-hi 120
   :key 8
   :melody {}
   :melody-char {:continuity 1, :density 7, :range 2, :smoothness 6}
   :metronome #<Metronome overtone.music.rhythm.Metronome@126f0939>
   :mm 94
   :player-id 3
   :prev-note-beat 0
   :scale :hindu
   :seg-len 10565
   :seg-start 0
   }

  {
   :behavior #transport.behavior.Behavior{:accuracy 0.7334685676866797, :action 3, :ensemble-action 0, :player-id 2}
   :cur-note-beat 0
   :function #<ensemble$play_melody transport.ensemble$play_melody@75a80485>
   :instrument-info :name saw-wave-sus
   :instrument-info :range-lo 17
   :instrument-info :range-hi 110
   :key 0
   :melody {}
   :melody-char {:continuity 9, :density 7, :range 4, :smoothness 5}
   :metronome #<Metronome overtone.music.rhythm.Metronome@37c686a3>
   :mm 115
   :player-id 4
   :prev-note-beat 0
   :scale :major
   :seg-len 16310
   :seg-start 0
   }
  )
