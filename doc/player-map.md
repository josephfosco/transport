*Documentation of :player map*

* :behavior - info about interactions with other players - contains
    - :action - interaction with other players (can be follow, contrast, ignore)
    - :player-id - player id of player that is being observed (optional only if :action is follow or contrast)
* :cur-note-beat - the beat the current note played was played on (starts at 0)
* :function - function to call when event is triggered
* :instrument-info - a map containing
    - :instrument -
    - :instrument-type -
    - :range-lo - the lowest note (midi note number)this instrument will play
    - :range-hi - the highest note (midi note number)this instrument will play
* :key - the key this player is playing in
* :melody - the melody played by this instrument. Is a map containing 0 or more maps of
    - :note - the midi note numger of the note played - nil is a rest
    - :dur-inf - a map of duration information for this note containing
      - :dur-millis - the duration of :note in milliseconds
      - :dur-note-dur - the duration of :note in beats (values are imdes into NOTE-DURS in rhythm.clj)
* :mm - BPM for this player
* :player-id - unique id for each player (does not change as long as transport is not paused
* :prev-note-beat - the beat the last note played was played on (starts at 0)
* :seg-len - the length of the player's current segment in milliseconds
* :seg-start - ?????????
* :scale - the scale this player is using
