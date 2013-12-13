* Document the :player map

* Rework rhythm.clj to use Overtone metronome instead of current method (if posssible) ???

* Rework event so schedule.clj does not have to access :function inside :player. Schedule.clj should not access anything in the data part of the event.

* problem next-sched-event-time in schedule.clj looks into event data

* Perhaps track lateness in play-melody in ensemle.clj

* Change SCALES to an atom or agent

* CHANGE scheduler-running to an atom or agent

* make create-player in ensemble.clj a closure that tracks player-id

* need to find a way to stop assuming :player-id is a sequential number starting with 1 in ensemble.clj

* pass in number of players from transport-init or transport-restart

* need to make certain ::dur-note-dur in get-dur-info-for-beats (rhythm.clj) is correct value
  currently it is no of beats. Should it be an index into the NOTE-DURS table or something else?

* what if when following we go to get the next event from the player we are following and it doesn't have it yet?
