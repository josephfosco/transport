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

(ns transport.util)

(defn get-max-map-key
  "For hash-maps with numeric keys, returns the highest key"
  [map]
  (reduce max 0 (keys map))
  )

(defmacro if-debug
  [& body]
  `(when ~'DEBUG ~@body)
  )

(defmacro print-msg
  "Prints a message of the format:
   *ns* - function msg
   where *ns* is the ns the calling function is in

   function: a string, generally the name of the calling function
   msg: one or more strings which will be concatenated together in the printed message"
  [function & msg]
  `(println (format "%-15s" (last (clojure.string/split (str ~*ns*) #"\." 2))) " -" ~function (str ~@msg))
  )
