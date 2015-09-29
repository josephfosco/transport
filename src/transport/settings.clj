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

(ns transport.settings
  "This namespace is a 'terminal namespace'.
   It should not :require :use :refer or :import any
   other transport namespaces"
    (:require [clojure.java.io])
)

(defn load-transport-config
  [file-name]
  (with-open [^java.io.Reader reader (clojure.java.io/reader file-name)]
    (let [props (java.util.Properties.)]
      (.load props reader)
      (into {} (for [[k v] props] [(keyword k) (read-string v)]))
      )
    )
  )

;; Load settings from config file
(def settings (load-transport-config "src/transport/config.properties"))


;; ALL config settings are loaded as atoms
(doseq [[k v] settings]
  (intern (ns-name *ns*) (symbol (name k)) (atom v))
  )

(defn get-setting
  [key]
  (key settings))

(defn reset-setting
  "Reset a setting  to new-val.
   returns - new-val

   setting-name - the name of the setting to be changed as a string
   new-val - new value for the setting"
  [setting-name new-val]
  (reset! (eval (symbol setting-name)) new-val))

(defn set-number-of-players
  [new-num-players]
  (reset! number-of-players new-num-players))
