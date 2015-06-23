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

(ns transport.util.track-trend
  (:require
   [transport.util.constants :refer :all]
   [transport.util.utils :refer [average]]
   ))

(defn track-trend
  []
  (let [vals (atom '())
        val-len (atom 0)
        change-threshold (atom 0)

        init (fn [new-vals new-threshold]
                 (dosync
                  (reset! vals new-vals)
                  (reset! val-len (count @vals))
                  (reset! change-threshold new-threshold)
                  )
               )

        new-value-to-track (fn [new-val]
                             (swap! vals #(conj (drop-last %1) %2) new-val)
                             )

        trend (fn []
                (let [diff (- (average @vals :list-length @val-len) (last @vals))]
                  (cond (> diff @change-threshold)
                        INCREASING
                        (> (* diff -1) @change-threshold)
                        DECREASING
                        :else
                        STEADY
                        )
                  )
                )

        print (fn []
                (println "vals:             "  @vals)
                (println "val-len:          "  @val-len)
                (println "change-threshold: "  @change-threshold)
                (println "average:          " (float (average @vals :list-length @val-len)))
                (println "diff:             " (float (- (average @vals :list-length @val-len) (last @vals))))
                )
        ]

    (fn [m]
      (cond
       (= m :new-value-to-track) new-value-to-track
       (= m :trend) trend
       (= m :print) print
       (= m :init) init
       )
      )))
