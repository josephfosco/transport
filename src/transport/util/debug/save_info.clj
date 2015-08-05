;    Copyright (C) 2015  Joseph Fosco. All Rights Reserved
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

(ns transport.util.debug.save-info)

(defn save-info
  []
  (let [save-len 100
        save-array (atom (vec (repeat save-len nil)))
        cur-ndx (atom 0)

        save (fn [new-element]
               (let [nxt-ndx (swap! cur-ndx #(mod (inc %1) save-len))]
                (reset! save-array (assoc @save-array nxt-ndx new-element)))
              )

        print (fn []
                (println "cur-ndx  :         "  @cur-ndx)
                (println "save-array:        "  @save-array)
                )
        ]

    (fn [m]
      (cond
       (= m :save) save
       (= m :print) print
       )
      )

    )
  )
