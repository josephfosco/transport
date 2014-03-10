;    Copyright (C) 2013  Joseph Fosco. All Rights Reserved
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

(ns transport.message_processor
  (:use
   [transport.settings]
   [transport.util :only [get-max-map-key]]
   ))

(def MESSAGES (agent {}))
(def LISTENERS (agent {}))
(def msg-id (atom (long  0)))
(def last-msg-processed (atom (long  0)))
(def checking-messages? (atom true)) ;; if false, message-processor is pause and will not watch MESSAGES

(defn- inc-msg-id
  [cur-msg-id]
  (inc cur-msg-id))

(declare process-messages)
(defn start-processing-messages
  [key identity old new]
  (if (> (get-max-map-key new) @last-msg-processed)  ;; if a new message has been added
    (do
      (remove-watch identity key)
      (process-messages))
    (println "MESSAGES NOT nil"))
  )

(defn- watch-msg-queue
  []
  (if @checking-messages?
    (do
      (add-watch MESSAGES :transport-start-processing-messages start-processing-messages)
      (println "Watching MESSAGES")
      true)    ; return true if watch is added
    (do
      (println "MESSAGE Watch NOT Added")
      false)))    ; return false if watch is not added

(defn send-message
  [msg]
  (if @checking-messages?
    (do
      (send-off MESSAGES assoc (swap! msg-id inc-msg-id) msg )
      true)    ;; return true if message was queued to be sent
    false      ;; return false if messsage was not queued
    )
  )

(defn- remove-msg
  "Removes a message from MESSAGES."
  [cur-msgs msg-num-to-remove]
  (dissoc cur-msgs msg-num-to-remove))

(defn- dispatch-message
  [msg-num & args]
  (let [msg-listeners (get @LISTENERS msg-num)]  ;; list of all listeners for msg-num
    (dotimes [lstnr-index (count msg-listeners)]
      (let [msg-lstnr (nth msg-listeners lstnr-index)]
        (if args
          (apply (first msg-lstnr) args (second msg-lstnr))
          (apply (first msg-lstnr) (second msg-lstnr))))))
  )

(defn process-messages
  []
  (let [nxt-msg (inc @last-msg-processed)]
    (while (not (nil? (get @MESSAGES nxt-msg)))
      (do
        (reset! last-msg-processed nxt-msg)
        (dispatch-message (get @MESSAGES nxt-msg))
        (send-off MESSAGES remove-msg nxt-msg)
        )))
  )

(defn start-message-processor
  "Starts the message-processor.
     If there are messages in MESSAGES it starts processing them.
     If there are no messages in MESSAGES it starts watching MESSAGES."
  []
  (if (> @msg-id @last-msg-processed)
    (process-messages)
    (watch-msg-queue)))

(defn stop-message-processor
  []
  "Stops the message-processor from adding any messages.
     Also,stops the message-processor from watching for new
     messsages.
     To get the message-processor going again call
     restart-message-processor."
  (reset! checking-messages? false)
  true    ;; return true
  )

(defn restart-message-processor
  []
  (reset! checking-messages? true))

(defn- add-listener
  "Called via send-off to add a listener to LISTENERS"
  [cur-listeners msg-num fnc args]
  (assoc cur-listeners msg-num (conj (get cur-listeners msg-num) (list fnc args)))
  )

(defn- remove-listener
  "Called via send-off to remove a listener from LISTENERS"
  [cur-listeners msg-num fnc args]
  (if (= 1 (count (get cur-listeners msg-num)))    ;; only 1 listener left in LOSTENERS
    (dissoc @LISTENERS msg-num)                    ;;   remove it
    (assoc
        cur-listeners
      msg-num
      (loop [lstnrs (get cur-listeners msg-num)
             new-lstnrs '()]
        (if (empty? lstnrs)
          new-lstnrs
          (if (= (first lstnrs) (list fnc (if (not (nil? args)) args nil)))  ;; is this the one to remove?
            (recur '()                                                       ;;   yes, remove it
                   (if (empty? (rest lstnrs))     ;;  removing last listener in list
                     new-lstnrs
                     (apply conj new-lstnrs (rest lstnrs))))
            (recur (rest lstnrs) (conj new-lstnrs (first lstnrs)))           ;;  no, add it to new list
            ))
        ))))

(defn register-listener
  "Register a function to bo called for a specific message

   msg-num - the message to be called for
   fnc - the function to be called
   args - an optional argument that is a map of key value pairs
          passed to fnc"
 [msg-num fnc & args]
 (send-off LISTENERS add-listener msg-num fnc args))

(defn unregister-listener
  [msg-num fnc & args]
  (send-off LISTENERS remove-listener msg-num fnc args)
)
