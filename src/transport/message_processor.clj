;    Copyright (C) 2013-2014, 2016  Joseph Fosco. All Rights Reserved
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

(ns transport.message-processor
  (:require
   [transport.messages :refer :all]
   [transport.util.print :refer [print-msg]]
   [transport.util.utils :refer [get-max-map-key]]
   ))

(def MESSAGES (agent {}))
(def LISTENERS (agent {}))
(def msg-id (atom (long  0)))
(def last-msg-processed (atom (long  0)))
(def checking-messages? (atom true)) ;; if false, message-processor is paused and will not watch MESSAGES

(defn- clear-messages
  [cur-msgs]
  {}
  )

(defn- clear-listeners
  [cur-lstnrs]
  {}
  )

(defn clear-message-processor
  "Clears messages and clears listeners"
  []
  (send MESSAGES clear-messages)
  (await MESSAGES)
  (send LISTENERS clear-listeners)
  (await LISTENERS)
  )

(declare process-messages)
(defn start-processing-messages
  [key identity old new]
  (if (> (get-max-map-key new) @last-msg-processed)  ;; if a new message has been added
    (do
      (remove-watch identity key)
      (process-messages))
    ;; else MESSAGES is empty - don't do anything
    )
  )

(defn- watch-msg-queue
  []
  (if @checking-messages?
    (do
      (add-watch MESSAGES :transport-start-processing-messages start-processing-messages)
      true)    ; return true if watch is added
    (do
      (println "MESSAGE Watch NOT Added")
      false)))    ; return false if watch is not added

(defn send-message
  "Sends a message to all listeners listening for msg.
   args must be key value pairs - args will be converted to a hash map

   args - sets of key value pairs to be passed to the listener"
  [msg & args]
  (if @checking-messages?
    (do
      (send MESSAGES assoc (swap! msg-id inc) (list msg (apply hash-map args)) )
      true)    ;; return true if message was queued to be sent
    false      ;; return false if messsage was not queued
    )
  )

(defn- remove-msg
  "Removes a message from MESSAGES."
  [cur-msgs msg-num-to-remove]
  (dissoc cur-msgs msg-num-to-remove))

(defn- dispatch-message-to-listener
  "Can be called with either a msg-lstnr list and msg-args or just msg-args
   Calls the msg-lstnr function with all args from the msg-lstnr and the msg"
  (
   [msg-lstnr msg-args]
     (if (nth msg-lstnr 2)                   ;; if message listener specified args
       (apply (first msg-lstnr)
              (flatten
               (list (flatten (seq msg-args)) (nth msg-lstnr 2))) ;; create one list from map msg-args and list
              )
       (apply (first msg-lstnr) (flatten (seq msg-args)))
       )
     )
  ([msg-lstnr]
     (if (nth msg-lstnr 2)                   ;; if message listener specified args
       (apply (first msg-lstnr) (nth msg-lstnr 2))
       ((first msg-lstnr))
       )
     )
  )

(defn- dispatch-message
  [msg-num args]
  (let [msg-listeners (get @LISTENERS msg-num)]  ;; list of all listeners for msg-num
    (dotimes [lstnr-index (count msg-listeners)]
      (let [msg-lstnr (nth msg-listeners lstnr-index)
            msg-lstnr-msg-args (second msg-lstnr)     ;; msg args the listner is watching
            ]
        (if (= msg-lstnr-msg-args {})                 ;; listener doesn't care about msg args
          (if (not= args {})
            (dispatch-message-to-listener msg-lstnr args)
            (dispatch-message-to-listener msg-lstnr)
            )
          (if (every? true? (map #(= (% msg-lstnr-msg-args) (% args)) (keys msg-lstnr-msg-args)))
            (dispatch-message-to-listener msg-lstnr args))  ;; only dispatch if keys match
          )
        )))
  )

(defn process-messages
  []
  (while (not (nil? (get @MESSAGES (inc @last-msg-processed))))
    (do
      (let [nxt-msg (swap! last-msg-processed inc)]
        (apply dispatch-message (first (get @MESSAGES nxt-msg)) (rest (get @MESSAGES nxt-msg)))
        (send MESSAGES remove-msg nxt-msg)
        )))
  (watch-msg-queue)
  )

(defn start-message-processor
  "Starts the message-processor.
     If there are messages in MESSAGES it starts processing them.
     If there are no messages in MESSAGES it starts watching MESSAGES."
  []
  (if (> @msg-id @last-msg-processed)
    (process-messages)
    (watch-msg-queue)))

(defn- set-atom-to-zero
  [cur-atom-val]
  0)

(defn stop-message-processor
  []
  "Stops the message-processor from adding any messages.
     Also,stops the message-processor from watching for new
     messsages.
     To get the message-processor going again call
     restart-message-processor."
  (reset! checking-messages? false)
  (send LISTENERS clear-listeners)     ;; clear LISTENERS
  (await MESSAGES)                     ;; wait till MESSAGE queue is cleared
  (swap! msg-id set-atom-to-zero)
  (swap! last-msg-processed set-atom-to-zero)
  true    ;; return true
  )

(defn restart-message-processor
  [& {:keys [reset-listeners]
      :or {reset-listeners false}}]
  (reset! checking-messages? true)
  (if reset-listeners
    (send LISTENERS clear-listeners)
    )
  )

(defn- add-listener
  "Called via send or send-off to add a listener to LISTENERS"
  [cur-listeners msg-num fnc msg-args args]
  (assoc cur-listeners msg-num (conj (get cur-listeners msg-num) (list fnc msg-args args)))
  )

(defn- remove-listener
  "Called via send or send-off to remove a listener from LISTENERS"
  [cur-listeners msg-num fnc msg-args args]
  (if (= 1 (count (get cur-listeners msg-num)))    ;; only 1 listener left in LISTENERS
    (dissoc @LISTENERS msg-num)                    ;;   remove it
    (assoc
        cur-listeners
      msg-num
      (loop [lstnrs (get cur-listeners msg-num)
             new-lstnrs '()]
        (if (empty? lstnrs)
          new-lstnrs
          (if (= (first lstnrs) (list
                                 fnc
                                 (if (not (nil? msg-args)) msg-args nil)
                                 (if (not (nil? args)) args nil)
                                 ))  ;; is this the one to remove?
            (recur '()                                                       ;;   yes, remove it
                   (if (empty? (rest lstnrs))     ;;  removing last listener in list
                     new-lstnrs
                     (apply conj new-lstnrs (rest lstnrs))))
            (recur (rest lstnrs) (conj new-lstnrs (first lstnrs)))           ;;  no, add it to new list
            ))
        ))))

(defn register-listener
  "Register a function to be called for a specific message

   msg-num - the message to be called for
   fnc - the function to be called
   msg-args - a map of keyword args to match with the args of msg-num
   args - an optional argument that is a map of key value pairs
          passed to fnc"
  [msg-num fnc msg-args & args]
  (send LISTENERS add-listener msg-num fnc msg-args args))

(defn unregister-listener
  [msg-num fnc msg-args & args]
  (send LISTENERS remove-listener msg-num fnc msg-args args)
)
