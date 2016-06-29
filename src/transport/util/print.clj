;    Copyright (C) 2013-2015  Joseph Fosco. All Rights Reserved
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

(ns transport.util.print)

(defmacro print-msg
  "Prints a message of the format:
   *ns* - function msg
   where *ns* is the ns the calling function is in

   function: a string, generally the name of the calling function
   msg: one or more strings which will be concatenated together in the printed message"
  [function & msg]
  `(println (format "%-15s" (last (clojure.string/split (str ~*ns*) #"\." 2))) " -" ~function "  " (str ~@msg))
  )

(defn print-banner
  "Prints a message with whatever is specified by the key :prefix around the message.
   If nothing is specified in :prefix '***' is used

   msg: the message to print"
  [msg & {:keys [prefix] :or {prefix "***"}}]
  (println " ")
  (println prefix)
  (println prefix msg)
  (println prefix)
  (println " ")
  )

(defn print-map
  "Pretty Print a map sorted by keys

  the-map - the map to print"
  [the-map & {:keys [ignore partial title]
             :or {ignore nil partial nil title nil}}]
  (let [sorted-keys (sort (keys the-map))]
    (doseq [map-key sorted-keys]
      (cond
       (comment
         (and (= player-key :instrument-info) (= prnt-full-inst-info false))
         )
       (comment
         (do
           (println (format "%-29s" (str "  " player-key " :name")) "-" (:name (:instrument (:instrument-info player))))
           (println (format "%-29s" (str "  " player-key " :range-lo")) "-" (:range-lo (:instrument-info player)))
           (println (format "%-29s" (str "  " player-key " :range-hi")) "-" (:range-hi (:instrument-info player))))
         )
       :else
       (println (format "%-20s" (str "  " map-key)) "-" (get the-map map-key)))
      )
    (prn)
    )
  )
