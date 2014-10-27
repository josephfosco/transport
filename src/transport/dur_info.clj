;    Copyright (C) 2013-2014  Joseph Fosco. All Rights Reserved
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

(ns transport.dur-info
  )

(defn get-dur-millis
  "Returns the duration in millis of dur-info

   dur-info - duration info to get dur-beats from"
  [dur-info]
  (:dur-millis dur-info)
  )

(defn get-dur-beats
  "Returns the duration in beats of dur-info

   dur-info - duration info to get dur-beats from"
  [dur-info]
  (:dur-beats dur-info)
  )
