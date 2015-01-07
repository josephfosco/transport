TRANSPORT
=========

# transport

This is the begining of Transport, a program that creates a kind of freely-improvised music.

This program is written in Clojure and uses the Overtone library. Overtone requires the synthesis library Supercollider

## Installation

Download from github.com

## Usage

To try the current version.

1. Install this software with it's dependencies.
- Start the application by opening a terminal window and cd to the directory where transport is installed
- At the terminal prompt type lein REPL
- If you use the REPL, at this time you can start playing but will not be able to stop due to status information being printed to the console.
- To be able to start and stop from the terminal, open an new terminal window (or tab), cd to the transport directory and at the terminal prompt enter lein repl :connect <port REPL is running in other terminal window>
- Enter transport commands in the new terminal window
- If you connect to the REPL using Emacs, you can start and stop from Emacs nrepl
- In the clojure REPL or the Emacs nrepl:
- make sure you are in the transport.core namespace - (ns transport.core)
- (transport-start)
- You should then hear notes being played.
- (transport-pause) in Emacs nrepl to stop playing notes - after all scheduled notes are played.
- To restart use either (transport-restart) or (transport-start) in Emacs nrepl
- (transport-help) to see instructions

## Options

transport-start command has a keyword arg :num-players to set the number of players.
For example (transport-start :num-players 15) to use 15 players.
If num-players is not set it will default to 10.

## Examples

... none yet ...

## License

Copyright © 2013-2014 Joseph Fosco

Distributed under the GNU General Public License
