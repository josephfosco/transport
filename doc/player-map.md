*Documentation of :player map*

* :behavior - info about interactions with other players - contains
    + :accuracy - this players accuracy
    + :action - interaction with other players (can be follow, complement, contrast, ignore)
    + :player-id - player id of player that is being observed (optional, only if :action is follow, complement or contrast)
* :function - function to call when event is triggered
* :instrument-info - nil if FOLLOWING else a map containing
    + :envelope-type - the type of envelope this instrument uses current vlaues
      - ASR  - attack sustein release
      - AD   - attack decay
    + :instrument -
    + :instrument-type -
    + :range-lo - the lowest note (midi note number)this instrument will play
    + :range-hi - the highest note (midi note number)this instrument will play
* :key - the key this player is playing in. A number between 0 and 11, 0 = C.
* :melody-char - a map of melody characteristics
    + density - how dense (continuous) the melody will be
        - 0 - sparse (discontinuous) (all rests)
        - 9 - dense (continuous) (few rests)
    + note-durs - rhythmic values of notes in melody
        - 0 - mostly notes of long duration
        - 9 - mostly notes of short duration
    + range - width of the melody's ramge in semitones
        - Range in semitones
    + pitch-smoothness - how stepwise the melody will be and the change in volume from note to note
        - 0 - mostly steps, same volume levels
        - 9 - mostly skips (wide skips), large volume differences
    + volume-smoothness - how consistent the volume will be note to note
        - 0 - consistent (similar) volume levels
        - 9 - large volume differences note to note
* :mm - BPM for this player
* :seg-len - the length of the player's current segment in milliseconds
* :seg-num - the number of the player's current segment
* :seg-start - the start time in millis of the player's current segment. If = 0 the next note will start a new segment
* :scale - the scale this player is using

*Documentation of :player-melodies map*

* :cur-note-beat - the beat the next note to be played will be played on (starts at 0)
* :cur-note-time - the time the next note to be played is scheduled to be played (starts at 0)
* :melody - the melody played by this instrument. The number of items in this map
            is set in SAVED-MELODY-LEN in settings.clj. This is a map containing 0
            or more of the following key-value pairs
    + key - the melody event number in numeric order starting at 1
    + value - a map containing
      - :note - the midi note number of the note played - nil is a rest
      - :dur-inf - a map of duration information for this note containing
        * :dur-millis - the duration of :note in milliseconds
        * :dur-note-dur - the duration of :note in beats
        * :follow-note - when FOLLOWing another player the melody-event this melody-event is in the FOLLOWers melody
      -  instrument-info - the instrument for this event
      - :volume - the volume level of this melody event. A value between 0 and 1
* :player-id - unique id for each player (does not change as long as transport is not paused
* :prev-note-beat - the beat the note that is playing was played on (starts at 0)
* :prev-note-time - the time the note that is playing was scheduled to be played (starts at 0)




Copyright 2013-2015  Joseph Fosco. All Rights Reserved
