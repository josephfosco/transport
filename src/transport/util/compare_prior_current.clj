;    Copyright (C) 2015  Joseph Fosco. All Rights Reserved
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.

;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.

(ns transport.util.compare-prior-current
  (:require
   [transport.util.util-constants :refer [DECREASING INCREASING STEADY]]
   ))

(defn compare-prior-current
  []
  (let [prior-val (atom 0)
        current-val (atom 0)
        change-threshold (atom 0)

        init (fn [new-threshold]
               (reset! prior-val 0)
               (reset! current-val 0)
               (reset! change-threshold new-threshold)
               )

        new-value (fn [new-val]
                    (if (or (> (- @current-val new-val) @change-threshold)
                            (> (- new-val @current-val) @change-threshold))
                      (do
                        (reset! prior-val @current-val)
                        (reset! current-val new-val)
                        )
                      (reset! prior-val @current-val))
                    )

        trend (fn []
                (let [diff (- @current-val @prior-val)]
                  (cond (> diff @change-threshold)
                        INCREASING
                        (> (* diff -1) @change-threshold)
                        DECREASING
                        :else
                        STEADY
                        )
                  )
                )

        ;; Returns the difference between current value and prior value
        trend-amount (fn []
                       (- @current-val @prior-val)
                       )

        print (fn []
                (println "current-val:       "  @current-val)
                (println "prior-val:         "  @prior-val)
                (println "change-threshold:  "  @change-threshold)
                )
        ]

    (fn [m]
      (cond
       (= m :new-value) new-value
       (= m :trend) trend
       (= m :trend-amount) trend-amount
       (= m :print) print
       (= m :init) init
       )
      )))
