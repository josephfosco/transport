;    Copyright (C) 2013  Joseph Fosco. All Rights Reserved
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.

(ns transport.messages)

;; player messages
(def MSG-PLAYER-NEW-SEGMENT 1)
(def MSG-PLAYER-NEW-FOLLOW-INFO 2)
(def MSG-PLAYER-NEW-COMPLEMENT-INFO 3)
(def MSG-PLAYER-NEW-CONTRAST-INFO 4)
(def MSG-PLAYER-NEW-NOTE 5)

;; ensemble status messages
(def MSG-LOUD-INTERUPT-EVENT 20)
