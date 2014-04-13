TRANSPORT
=========

# transport

This is the begining of Transport, a program that creates free improvisation music.

This program is written in Clojure and uses the Overtone library. Overtone requires the synthesis library Supercollider

## Installation

Download from github.com

## Usage

To try the current version.

1. Install this software with it's dependencies.
2. If you use the REPL, at this time you can start playing but will not be able to start
3. If you connect to the REPL using Emacs, you can start and stop from Emacs nrepl
4. In the clojure REPL or the Emacs nrepl:
5. make sure you are in the transport.core namespace - (ns transport.core)
6. (transport-start)
7. You should then hear notes being played.
8. (transport-pause) in Emacs nrepl to stop playing notes - after all scheduled notes are played.
9. To restart use either (transport-restart) or (transport-start) in Emacs nrepl
10. (transport-help) to see instructions

## Options

transport-start command has a keyword arg :num-players to set the number of players.
For example (transport-start :num-players 15) to use 15 players.
If num-players is not set it will default to 10.

## Examples

... none yet ...

## License

Copyright © 2013 Joseph Fosco

Distributed under the GNU General Public License
