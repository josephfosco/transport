* Document the :player map

* Rework rhythm.clj to use Overtone metronome instead of current method (if posssible) ???

* Use at- function from Overtone in ensemble.clj play-melody for more accurate playback
  - Will require some type of latency compensation in ensemble.clj to schedule sooner than neede

* Change SCALES to an atom or agent

* CHANGE scheduler-running to an atom or agent

* make create-player in ensemble.clj a closure that tracks player-id

* need to find a way to stop assuming :player-id is a sequential number starting with 1 in ensemble.clj

* need to find a way so that the first few time through checking COMPLEMENT rhythms that the average is not computed from the initial 0's in rhythm-values-millis (ensemble-status.clj)

* Add print-lateness-fl for schedule.clj to settings.clj

* find a way to have an overall level adjustment based on the number of players

* Add :num-players to transport-start in core.clj

* If a pleyer is FOLLOWing another, needd to know if the FOLLOWer player changes instrument

* Need to implement a message queue for sending information

* In pitch.clj get-step-down-in-scale and get-step-up-in-scale need to return nil if pitch passed in is not in player scale and key

* no volume 0 (or a minimum volumem maybe .1)
