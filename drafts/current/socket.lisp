(use-package :sb-bsd-sockets)

(defvar socket (make-instance 'inet-socket :type :stream :protocol :tcp))

(socket-bind socket (make-inet-address "0.0.0.0") 8081)
(socket-listen socket 5)

(defvar conn-socket)

(setf conn-socket (socket-accept socket))

(defvar client-socket
  (make-instance 'inet-socket :type :stream :protocol :tcp))

(socket-connect client-socket #(127 0 0 1) 8081)

(let ((msg "world"))
  (socket-send client-socket msg (length msg)))

(defvar buf (make-array 100 :element-type 'character))

(socket-receive conn-socket buf 5)
