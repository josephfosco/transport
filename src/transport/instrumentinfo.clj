;    Copyright (C) 2014  Joseph Fosco. All Rights Reserved
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

(ns transport.instrumentinfo)

(defrecord InstrumentInfo [instrument envelope-type release-dur range-hi range-lo])

(defn create-instrument-info
  "Used to create an InstrumentInfo record"
  [& {:keys [instrument envelope-type release-dur range-hi range-lo]}]
  (InstrumentInfo. instrument
                   envelope-type
                   release-dur
                   range-hi
                   range-lo
                   )
  )

(defn get-instrument-for-inst
  [inst-info]
  (:instrument inst-info))

(defn get-envelope-type-for-inst
  [inst-info]
  (:envelope-type inst-info))

(defn get-release-dur-for-inst
  [inst-info]
  (:release-dur inst-info))

(defn get-range-hi-for-inst
  [inst-info]
  (:range-hi inst-info))

(defn get-range-lo-for-inst
  [inst-info]
  (:range-lo inst-info))
