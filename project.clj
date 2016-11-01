;    Copyright (C) 2013-2016  Joseph Fosco. All Rights Reserved
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

(defproject transport "0.4.0-SNAPSHOT"
  :description "TRANSPORT - an improvisational music system"
  :url "http://example.com/FIXME"
  :license {:name "GNU General Public License version 3"
            :url "http://www.gnu.org/licenses/"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [overtone "0.10.1"]
                 ]
  :jvm-opts ^:replace [] ;; turns off JVM arg TieredStopAtLevel=1
  :main transport.core)
