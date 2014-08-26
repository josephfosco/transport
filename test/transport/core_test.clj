;    Copyright (C) 2014  Joseph Fosco. All Rights Reserved
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

;; To run tests:
;; 1. Load REPL with transport
;; 2. Load core_test.clj in REPL
;; 3. (ns transport.core-test)
;; 4. (run-tests)

(ns transport.core-test
  (:require
   [clojure.test :refer :all]
   [overtone.live :refer [metronome]]
   [transport.behavior :refer :all]
   [transport.core :refer :all]
   [transport.ensemble :refer [play-melody]]
   [transport.instrument :refer [select-random-instrument]]
   [transport.melodychar :refer :all]
   [transport.players :refer :all]
   [transport.schedule :refer [clear-scheduler]]
   )
  (:import
   transport.melodychar.MelodyChar
   transport.behavior.Behavior
   )
  )

(comment
  (deftest a-test
    (testing "FIXME, I fail."
      (is (= 0 1))))
  )

(deftest t01-note-dur-to-milllis
  (testing "t01-test-note-dur-to-millis"
    (is (= (note-dur-to-millis 60 1) 1000)
        )))

(deftest t02-note-dur-to-milllis
  (testing "t02-test-note-dur-to-millis"
    (is (= (note-dur-to-millis 120 1/2) 250)
        )))

(deftest t03-milllis-to-note-dur
  (testing "t03-test-note-dur-to-millis"
    (is (= (millis-to-note-dur 120 250) 0.5)
        )))

(deftest t04-compute-mm-from-dur-info
  (testing "t04-compute-mm-from-dur-info"
    (is (= (compute-mm-from-dur-info 1000 1) 60)
        )))

(deftest t05-compute-mm-from-dur-info
  (testing "t05-compute-mm-from-dur-info"
    (is (= (compute-mm-from-dur-info 250 1/2) 120)
        )))

(deftest t06-compute-mm-from-dur-info
  (testing "t06-compute-mm-from-dur-info"
    (is (= (compute-mm-from-dur-info 1915 3) 94)
        )))

(deftest follow-test-1
  (testing "Player 0 follows player 1 first time"
    (let [tst-players
          {
           0
           {
            :behavior (Behavior. 0.2966165302092916, 3, 0, 1)
            :change-follow-info-note 0
            :cur-note-beat 0
            :function  transport.ensemble/play-melody
            :instrument-info (select-random-instrument)
            :key 0
            :melody {}
            :melody-char (MelodyChar. 9, 4, 2, 0)
            :metronome (metronome 64)
            :mm 64
            :player-id 0
            :prev-note-beat 0
            :scale :ionian
            :seg-len 19726
            :seg-start 0
            }
           1
           {
            :behavior (Behavior. 0.4798252752058973, 0, 0, nil)
            :change-follow-info-note nil
            :cur-note-beat 0
            :function transport.ensemble/play-melody
            :instrument-info(select-random-instrument)
            :key 8
            :melody {}
            :melody-char (MelodyChar. 1, 7, 2, 6)
            :metronome (metronome 94)
            :mm 94
            :player-id 1
            :prev-note-beat 0
            :scale :hindu
            :seg-len 10565
            :seg-start 0
            }
           }
          ]

      (reset-players)
      (send PLAYERS conj tst-players)
      (await PLAYERS)

      (is (= true
             (try
               (play-melody 0 0)
               (transport-clear)
               (println "PASS - Initial Follow test - PASS")
               true
               (catch Exception e
                 (println "ERROR - Exception in Initial Follow test - ERROR")
                 (println e)
                 (transport-clear)
                 false
                 )
               )
             ))

      )
    ))
