;;;; util.lisp
;;;;
;;;; This file is part of the cl-mongrel library, released under BSD license.
;;;; See file LICENSE for permissions.
;;;;
;;;; Authors: Moskvitin Andrey <archimag@gmail.com>

(in-package #:mongrel2)

(defclass reply ()
  ((headers-out :initform nil :accessor headers-out)
   (return-code :initform 200 :accessor return-code)
   (return-status :initform "Ok" :accessor return-status)
   (external-format :initform :utf-8 :accessor reply-external-format)))

(defgeneric reply (connection request reply data))

(defparameter +crlf+
  (format nil "~C~C" #\Return #\Linefeed))

(defmethod reply (connection request (reply reply) (data string))
  (setf (slot-value reply 'headers-out)
        (list (cons :content-length (length data))
              (cons :content-type "text/html")))
  (reply connection
         request
         nil
         (with-output-to-string (out)
           (format out "HTTP/1.1 ~A ~A" (return-code reply) (return-status reply))
           (write-string +crlf+ out)
           (iter (for (head . value) in (headers-out reply))
                 (iter (for ch in-string (string head))
                       (for prev previous ch)
                       (write-char (if (or (not prev)
                                           (char= prev #\-))
                                       ch
                                       (char-downcase ch))
                                   out)
                       )
                 (write-string ": " out)
                 (format out "~A" value)
                 (write-string +crlf+ out))
           (write-string +crlf+ out)
           (write-string data out))))

(defmethod reply (connection request (reply null) (data string))
  (zmq:send (connection-resp-socket connection)
            (make-instance 'zmq:msg
                           :data (format nil
                                         "~A ~A:~A, ~A"
                                         (sender-uuid connection)
                                         (length (connection-id request))
                                         (connection-id request)
                                         data))))

