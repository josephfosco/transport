*Documentation of :player map*

* :behavior - info about interactions with other players - contains
    - :accuracy - this players accuracy
    - :action - interaction with other players (can be follow, complement, contrast, ignore)
    - :player-id - player id of player that is being observed (optional only if :action is follow or contrast)
* :cur-note-beat - the beat the next note to be played will be played on (starts at 0)
* :function - function to call when event is triggered
* :instrument-info - a map containing
    - :envelope-type - the type of envelope this instrument uses current vlaues
      - ASR  - attack sustein release
      - AD   - attack decay
    - :instrument -
    - :instrument-type -
    - :range-lo - the lowest note (midi note number)this instrument will play
    - :range-hi - the highest note (midi note number)this instrument will play
* :key - the key this player is playing in
* :melody - the melody played by this instrument. The number of items in this map
            is set in SAVED-MELODY-LEN in settings.clj. This is a map containing 0 or more maps of
    - key - the melody event number in numeric order starting at 1
    - value - a map containing
      - :note - the midi note numger of the note played - nil is a rest
      - :dur-inf - a map of duration information for this note containing
        - :dur-millis - the duration of :note in milliseconds
        - :dur-note-dur - the duration of :note in beats
        - :follow-note - when FOLLOWing another player the melody-event this melody-event is in the FOLLOWers melody
      - :volume - the volume level of this melody event
* :mm - BPM for this player
* :player-id - unique id for each player (does not change as long as transport is not paused
* :prev-note-beat - the beat the  note that is playing played was played on (starts at 0)
* :seg-len - the length of the player's current segment in milliseconds
* :seg-start - ?????????
* :scale - the scale this player is using
