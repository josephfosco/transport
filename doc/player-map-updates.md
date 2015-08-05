*Documentation of when items in :player map change*

* :behavior
    + segment - new-segment
    + ensemble - init-ensemble: sets behavior player-id if necessary
* :change-follow-info-note
    + segment - new-segment
    + segment - first-segment
    + ensemble - play-melody: when new segment for following player
    + players - new-change-follow-info-note: called when processsing a MSG-NEW-FOLLOW-INFO
* :cur-note-beat
    + ensemble - update-player-info: called for every event in play-melody
    + ensemble - create-player: called from init-ensemble
    + segment - new-segment
* :cur-note-time
    + ensemble - update-player-info: called for every event in play-melody
    + ensemble - create-player: called from init-ensemble
* :function
    + ensemble - create-player: called from init-ensemble
    + ensemble - play-first-melody-note: points function to play-melody
* :instrument-info
    + segment - first-segment
    + segment - new-segment
?    + players - new-change-follow-info-note: called when processsing a MSG-NEW-FOLLOW-INFO
?    + players - new-contrast-info-for-player: called when processing MSG-PLAYER-NEW-CONTRAST-INFO
* :key
    + segment - first-segment
    + segment - new-segment
* :melody
    + ensemble - update-player-info: called for every event in play-melody
    + ensemble - create-player: called from init-ensemble
* :melody-char
    + players - new-change-follow-info-note: called when processsing a MSG-NEW-FOLLOW-INFO
    + players - new-change-follow-info-note: called when processsing a MSG-NEW-SIMILAR-INFO
* :mm
    + players - new-change-follow-info-note: called when processsing a MSG-NEW-FOLLOW-INFO
    + players - new-change-follow-info-note: called when processsing a MSG-NEW-SIMILAR-INFO
    + segment - first-segment or new-segment, not FOLLOW-PLAYER or SIMILAR-PLAYER
* :player-id
    + ensemble - create-player: called from init-ensemble
* :prev-note-beat
    + ensemble - update-player-info: called for every event in play-melody
    + ensemble - create-player: called from init-ensemble
* :prev-note-time
    + ensemble - update-player-info: called for every event in play-melody
    + ensemble - create-player: called from init-ensemble
* :seg-len
    + segment - first-segment or new-segment
* :seg-num
    + segment - first-segment or new-segment
* :seg-start
    + ensemble - update-player-info: called for every event in play-melody
    + segment - first-segment and new-segment both set to 0
* :scale
    + players - new-change-follow-info-note: called when processsing a MSG-NEW-FOLLOW-INFO
    + players - new-change-follow-info-note: called when processsing a MSG-NEW-SIMILAR-INFO
    + segment - new-segment
* :sync-beat-player-id
    + ensemble - update-player-info: called for every event in play-melody (always set to nil)
    + ensemble - create-player: called from init-ensemble (always set to nil)
    + segment - new-segment (FOLLOW-PLAYER or SIMILAR-ENSEMBLE)

Copyright 2015  Joseph Fosco. All Rights Reserved
