* Document the :player map

* Rework rhythm.clj to use Overtone metronome instead of current method (if posssible) ???

* Rework event so schedule.clj does not have to access :function inside :player. Schedule.clj should not access anything in the data part of the event.

* problem next-sched-event-time in schedule.clj looks into event data

* Perhaps track lateness in play-melody in ensemle.clj
