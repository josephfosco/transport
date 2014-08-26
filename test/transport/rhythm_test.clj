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

(ns transport.rhythm-test
  (:require
   [clojure.test :refer :all]
   [transport.rhythm :refer :all]
   )
  )

(deftest t01-note-dur-to-milllis
  (testing "t01-test-note-dur-to-millis"
    (is (= (note-dur-to-millis 60 1) 1000)
        )))

(deftest t02-note-dur-to-milllis
  (testing "t02-test-note-dur-to-millis"
    (is (= (note-dur-to-millis 120 1/2) 250)
        )))

(deftest t03-milllis-to-note-dur
  (testing "t03-test-note-dur-to-millis"
    (is (= (millis-to-note-dur 120 250) 0.5)
        )))

(deftest t04-compute-mm-from-dur-info
  (testing "t04-compute-mm-from-dur-info"
    (is (= (compute-mm-from-dur-info 1000 1) 60)
        )))

(deftest t05-compute-mm-from-dur-info
  (testing "t05-compute-mm-from-dur-info"
    (is (= (compute-mm-from-dur-info 250 1/2) 120)
        )))

(deftest t06-compute-mm-from-dur-info
  (testing "t06-compute-mm-from-dur-info"
    (is (= (compute-mm-from-dur-info 1915 3) 94)
        )))
