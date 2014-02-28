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
   ))

(def MESSAGES (agent {}))
(def LISTENERS (agent {}))
(def FN-LIST (agent {}))
(def msg-no (atom (long  0)))
(def last-msg-processed (atom (long  0)))
(def checking-messages? false)

;; message type no data
;; message-type message-no func

(defn inc-msg-no
  [cur-msg-val]
  (inc cur-msg-val))

(declare process-messages)
(defn start-processing-messages
  [key identity old new]
  (remove-watch MESSAGES :transport-start-processing-messages)
  (if (= (count  old) 0)
    (process-messages (first new))
    (println "MESSAGES NOT nil")))

(defn watch-msg-queue
  []
  (if (not checking-messages?)
    (do
      (add-watch MESSAGES :transport-start-processing-messages start-processing-messages)
      (println "Watching MESSAGES")
      true)    ; return true if watch is added
    (do
      (println "MESSAGE Watch NOT Added")
      false)))    ; return false if watch is not added

(defn inc-last-msg-processed
  [cur-last-msg-proc]
  (inc cur-last-msg-proc))

(defn add-message
  [msg]
  (send-off MESSAGES assoc (swap! msg-no inc-msg-no) msg ))

(defn remove-msg
  "Removes amessage from MESSAGES."
  [cur-msgs msg-num-to-remove]
  (dissoc cur-msgs msg-num-to-remove))

(defn dispatch-message
  [msg]
  (println "message dispatched!!! msg:" msg)
  )
(defn process-messages
  []
  (while (not (nil? (get @MESSAGES (inc @last-msg-processed))))
    (do
      (swap! last-msg-processed inc-last-msg-processed)
      (dispatch-message (get @MESSAGES @last-msg-processed))
      (send-off MESSAGES remove-msg @last-msg-processed)
      ))
  )

(defn start-scheduler
  "Starts the music message processor.
     If there are messages in the message queue it starts processing the queue.
     If there are no messages in the queue it starts watching the queue."
  []
  (if (> (count @MESSAGES) 0)
    (process-messages)
    (watch-msg-queue)))

(defn add-listener
  "Called via send-off to add a listener to LISTENERS"
  [cur-listeners msg-num fnc args]
  (assoc cur-listeners msg-num (conj (get cur-listeners msg-num) (list fnc args)))
  )

(defn remove-listener
  "Called via send-off to remove a listener from LISTENERS"
  [cur-listeners msg-num fnc args]
  (if (= 1 (count (get cur-listeners msg-num)))
    (dissoc @LISTENERS msg-num)
    (assoc
        cur-listeners
      msg-num
      (loop [lstnrs (get cur-listeners msg-num)
             new-lstnrs '()]
        (if (= (count lstnrs) 0)
          new-lstnrs
          (if (= (first lstnrs) (list fnc (if (not (nil? args)) args nil)))
            (recur '() (apply conj new-lstnrs (rest  lstnrs)))
            (recur (rest lstnrs) (conj new-lstnrs (first lstnrs)))
            ))
        ))))

(defn register-listener
 [msg-num fnc & args]
 (send-off LISTENERS add-listener msg-num fnc args))

(defn unregister-listener
  [msg-num fnc & args]
  (send-off LISTENERS remove-listener msg-num fnc args)
)
