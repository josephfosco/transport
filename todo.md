* Rework rhythm.clj to use Overtone metronome instead of current method (if posssible) ???

* Use at- function from Overtone in ensemble.clj play-melody for more accurate playback
  - Will require some type of latency compensation in ensemble.clj to schedule sooner than neede

* make create-player in ensemble.clj a closure that tracks player-id

* need to find a way to stop assuming :player-id is a sequential number starting with 1 in ensemble.clj

* need to find a way so that the first few time through checking COMPLEMENT rhythms that the average is not computed from the initial 0's in rhythm-values-millis (ensemble-status.clj)

* Add print-lateness-fl for schedule.clj to settings.clj

* find a way to have an overall level adjustment based on the number of players

* In pitch.clj get-step-down-in-scale and get-step-up-in-scale need to return nil if pitch passed in is not in player scale and key

* no volume 0 (or a minimum volumem maybe .1)

* need some type of CLI or separate window for messages

* remove duplicate @SCALES

* move :accuracy out of :behavior up to top level

* possibly move next note out of ensemble into player (each player knows when to play next)

* make each player a record (defrecord) for better performance

* find a way to not call process-messages in message-processor when messages are removed from queue

* need to work on ending

* Could FOLLOW/COMPLEMENT/CONTRAST up or down an octave or other interval

* change message processor to use type in addition to message number

* make melody events records

* possibly helpful to track mm changes in melody - (can get this now from dur-millis and dur-note)

* Find a way to sync players to match beats not just tempo
