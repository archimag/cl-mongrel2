;;;; connection.lisp
;;;;
;;;; This file is part of the cl-mongrel library, released under BSD license.
;;;; See file LICENSE for permissions.
;;;;
;;;; Authors: Vsevolod Dyomkin <vseloved@gmail.com>, Moskvitin Andrey <archimag@gmail.com>

(in-package #:mongrel2)

(defclass connection ()
  ((sender-uuid :initarg :sender-uuid :type string
                :reader sender-uuid
                :initform (error "Should provide sender UUID"))
   (req-socket :initarg :req-socket
               :accessor connection-req-socket)
   (resp-socket :initarg :resp-socket
                :accessor connection-resp-socket)
   (sub-addr :initarg :sub-addr
             :reader connection-sub-addr
             :initform (error "Should provide SUB addr"))
   (pub-addr :initarg :pub-addr
             :reader connection-pub-addr
             :initform (error "Should provide PUB addr")))
  (:documentation
   "A Connection object manages the connection between your handler
    and a Mongrel2 server (or servers).  It can receive raw requests
    or JSON encoded requests whether from HTTP or MSG request types,
    and it can send individual responses or batch responses either
    raw or as JSON.  It also has a way to encode HTTP responses
    for simplicity since that'll be fairly common."))


(defmacro with-connection ((conn &key sender-uuid sub-addr pub-addr)
                           &body body)
  (let ((ctx (gensym))
        (req-socket (gensym))
        (resp-socket (gensym)))
    `(let ((,conn (make-instance 'connection
                                 :sender-uuid ,sender-uuid
                                 :sub-addr ,sub-addr
                                 :pub-addr ,pub-addr)))
       (with-slots (sender-uuid sub-addr pub-addr)
           ,conn
         (zmq:with-context (,ctx 1)
           (zmq:with-socket (,req-socket ,ctx zmq:upstream)
             (zmq:connect ,req-socket sub-addr)
             (setf (connection-req-socket ,conn) ,req-socket)
             (zmq:with-socket (,resp-socket ,ctx zmq:pub)
               (setf (connection-resp-socket ,conn) ,resp-socket)
               (zmq:connect ,resp-socket pub-addr)
               (zmq:setsockopt ,resp-socket zmq:identity sender-uuid)
               ,@body)))))))

(defun recv (conn)
  (let ((msg (make-instance 'zmq:msg)))
    (zmq:recv (connection-req-socket conn) msg)
    (make-instance 'request :msg msg)))

(defun send (conn conn-id msg)
  (fmt-mongrel conn "~A:~A, ~A" (length (princ-to-string conn-id)) conn-id msg))

(defmethod reply (conn req msg)
  (send conn (connection-id req) msg))

;; (defmethod deliver (conn conn-ids msg)
;;   (fmt-mongrel conn "~A:~{~A~}, ~A"
;;                (reduce #'+ (mapcar (lambda (str)
;;                                      (length (princ-to-string str)))
;;                                    conn-ids)
;;                        :initial-value (1- (length conn-ids)))
;;                (fmt "~A " conn-ids) msg))

;;; end

