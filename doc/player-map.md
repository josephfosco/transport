*Documentation of :player map*

* :dur - ????????????
* :function - function to call when event is triggered
* :instrument-info - a map containing
    ?????
* :key - the key this player is playing in
* :melody - the melody played by this instrument. Is a map containing 0 or more maps of
    - :note - the midi note numger of the note played
    - :dur - the duration of :note in milliseconds
* :mm - BPM for this player
* :player-id - unique id for each player (does not change as long as transport is not paused
* :seg-len - the length of the player's current segment in milliseconds
* :seg-start - ?????????
* :scale - the scale this player is using
