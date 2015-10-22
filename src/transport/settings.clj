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
  (:require [clojure.java.io :refer [as-file]])
)

(def settings (atom {}))

(defn load-transport-config
  [file-name]
  (with-open [^java.io.Reader reader (clojure.java.io/reader file-name)]
    (let [props (java.util.Properties.)]
      (.load props reader)
      (into {} (for [[k v] props] [(keyword k) (read-string v)]))
      )
    )
  )

(defn init-settings-from-config
  "Load settings from config file(s)"
  []
  (let [config-files (if (.exists (as-file "config_files.properties"))
                       (load-transport-config "config_files.properties")
                       {})]
    ;; first load default settings
    (reset! settings (load-transport-config "./src/transport/config/config.properties"))
    ;; then add or overwrite with custom settings
    (when (:custom-config config-files)
      (reset! settings (merge @settings (load-transport-config (str (:custom-config config-files))))))
    )

  ;; after all config files are loaded, the final settings are loaded as atoms
  (doseq [[k v] @settings]
    (intern (ns-name *ns*) (symbol (name k)) (atom v))
    )
  )

;; initialize settings when this file loads
(init-settings-from-config)

(defn get-setting
  "returns the value of a setting.
   Returns nil if the setting does not exist

   setting - the setting name as a string"
  [setting]
  (try
    (deref (eval (symbol setting)))
    (catch RuntimeException e
      nil)
    )
  )

(defn reset-setting
  "Reset a setting  to new-val.
   returns - new-val or nil if the setting does not exist

   setting-name - the name of the setting to be changed as a string
   new-val - new value for the setting"
  [setting-name new-val]
  (try
    (reset! (eval (symbol setting-name)) new-val)
    (catch RuntimeException e
      nil))
    )


(defn set-number-of-players
  [new-num-players]
  (reset! number-of-players new-num-players))
