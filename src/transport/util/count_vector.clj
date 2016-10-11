;    Copyright (C) 2014, 2016  Joseph Fosco. All Rights Reserved
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

(ns transport.util.count-vector)

(defn count-vector
  []
  (let [cnt-vctr (atom [])
        eval-fnc (atom nil)

        set-vector (fn [vector]
               (reset! cnt-vctr vector)
               )

        set-eval (fn [fnc]
                   (reset! eval-fnc fnc)
                   )

        init (fn [vector fnc]
               (set-vector vector)
               (set-eval fnc)
               )

        inc (fn [& args]
              (let [inc-index (apply @eval-fnc args)]
                (reset! cnt-vctr (assoc @cnt-vctr inc-index (inc (get @cnt-vctr inc-index)))))
              )

        print (fn []
                (println "count-vector:" @cnt-vctr))

        get (fn []
              @cnt-vctr)

        ]

    (fn [m]
      (cond
       (= m :inc) inc
       (= m :get) get
       (= m :init) init
       (= m :set-vector) set-vector
       (= m :set-eval) set-eval
       (= m :print) print
       )
      )
    ))
