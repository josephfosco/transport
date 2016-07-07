;    Copyright (C) 2016  Joseph Fosco. All Rights Reserved
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

(ns transport.util.log
  (:import [java.util.logging Logger Level ConsoleHandler FileHandler
            StreamHandler Formatter LogRecord])
  (:require [transport.settings :refer [get-setting]]
            ))

(def ^:private LOG-LEVELS {:data2 Level/FINEST
                           :data  Level/FINER
                           :debug Level/FINE
                           :info  Level/INFO
                           :warn  Level/WARNING
                           :error Level/SEVERE
                           })
(def ^:private DEFAULT-LOG-LEVEL :warn)
(def ^:private PRINT_LOG_LEVEL (atom false))
(def ^:private LOGGER (Logger/getLogger "transport"))
(defonce ^:private LOG-CONSOLE (ConsoleHandler.))

(defmacro format-msg
  "formats msg as:
   *ns* - function msg
   where *ns* is the ns the calling function is in

   function: a string, generally the name of the calling function
   msg: one or more strings which will be concatenated together in the printed message"
  [function-name & msg]
  `(str (format "%-15s" (last (clojure.string/split (str ~*ns*) #"\." 2))) " - " ~function-name "  " (str ~@msg))
 )
(defn set-level!
  "Set the log level. Valid levels are :data2, :data, :debug, :info,
   :warn  or :error"
  [level]
  (assert (contains? LOG-LEVELS level))
  (.setLevel LOGGER (get LOG-LEVELS level))
  )

(defn set-print-log-level!
  [val]
  (reset! PRINT_LOG_LEVEL (if val true false))
  )

(defn data2
  [& msg]
  (.log LOGGER Level/FINEST (apply str (if @PRINT_LOG_LEVEL "DATA2: ") msg))
  )

(defn data
  [& msg]
  (.log LOGGER Level/FINER (apply str (if @PRINT_LOG_LEVEL "DATA:  ") msg))
  )

(defn debug
  [& msg]
  (.log LOGGER Level/FINE (apply str (if @PRINT_LOG_LEVEL "DEBUG: ") msg))
  )

(defn info
  [& msg]
  (.log LOGGER Level/INFO (apply str (if @PRINT_LOG_LEVEL "INFO:  ") msg))
  )

(defn warn
  [& msg]
  (.log LOGGER Level/WARNING (apply str (if @PRINT_LOG_LEVEL "WARN:  ") msg))
  )

(defn error
  [& msg]
  (.log LOGGER Level/SEVERE (apply str (if @PRINT_LOG_LEVEL "ERROR: ") msg))
  )

(defn- initial-log-level
  []
  (or (get-setting "log-level")
      DEFAULT-LOG-LEVEL))

(defn- log-formatter
  []
  (proxy [Formatter] []
    (format [^LogRecord log-rec]
      (.getMessage log-rec)
      )))

(defn- print-handler
  []
  (let [formatter (log-formatter)]
    (proxy [StreamHandler] []
      (publish [msg] (println (.format formatter msg))))
    )
  )

;; setup logger
(defonce ^:private __setup-logs__
  (do
    (set-level! (initial-log-level))
    (.addHandler LOGGER (print-handler))
    (.setUseParentHandlers LOGGER false)
    )
  )

(defonce ^:private __cleanup-logger-on-shutdown__
  (.addShutdownHook (Runtime/getRuntime)
                    (Thread. (fn []
                               (info "Shutting down - cleaning up logger")
                               (.removeHandler LOGGER LOG-CONSOLE)
                               (.close LOG-CONSOLE)
                               ))
                    )
  )
