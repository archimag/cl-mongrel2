;;;; util.lisp
;;;;
;;;; This file is part of the cl-mongrel library, released under BSD license.
;;;; See file LICENSE for permissions.
;;;;
;;;; Authors: Moskvitin Andrey <archimag@gmail.com>

(in-package #:mongrel2)

(defclass reply ()
  ((headers-out :initform nil :accessor wsal:headers-out)
   (return-code :initform wsal:+http-ok+ :accessor wsal:return-code)
   (cookies-out :initform nil :accessor wsal:cookies-out)
   (external-format :initform :utf-8 :accessor wsal:reply-external-format)))

(defmethod initialize-instance :after ((reply reply) &key)
  (setf (wsal:header-out :content-type reply) 
        "text/html"))

(defmethod wsal:content-type ((reply reply))
  (wsal:header-out :content-type reply))

(defmethod wsal:content-length ((reply reply))
  (wsal:header-out :content-length reply))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; send reply
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric reply (connection request reply data))

(defparameter +crlf+
  (format nil "~C~C" #\Return #\Linefeed))

(defun format-reply-string (reply &optional data)
  (with-output-to-string (out)
    (format out
            "HTTP/1.1 ~A ~A"
            (wsal:return-code reply)
            (wsal:reason-phrase (wsal:return-code reply)))
    (write-string +crlf+ out)
    (iter (for (head . value) in (wsal:headers-out reply))
          (iter (for ch in-string (string head))
                (for prev previous ch)
                (write-char (if (or (not prev)
                                    (char= prev #\-))
                                ch
                                (char-downcase ch))
                            out))
          (write-string ": " out)
          (format out "~A" value)
          (write-string +crlf+ out))
    (write-string +crlf+ out)
    (when data
      (write-string data out))))

(defmethod reply (connection request (reply reply) (data string))
  (setf (wsal:content-length* reply)
        (length data))
  (reply connection
         request
         nil
         (format-reply-string reply data)))

(defmethod reply (connection request reply (octets vector))
  (check-type octets (vector (unsigned-byte 8)))
  (setf (wsal:content-length* reply)
        (length octets))
  (reply connection request reply nil)
  (reply connection request nil octets))

(defmethod reply (connection request (reply null) (octets vector))
  (check-type octets (vector (unsigned-byte 8)))
  (let ((bufsize 100))
    (iter (for pos from 0 below (length octets) by bufsize)
          (zmq:send (connection-resp-socket connection)
                    (make-instance 'zmq:msg
                                   :data (concatenate '(vector (unsigned-byte 8) *)
                                                      (babel:string-to-octets (format nil
                                                                                      "~A ~A:~A, "
                                                                                      (sender-uuid connection)
                                                                                      (length (connection-id request))
                                                                                      (connection-id request))
                                                                              :encoding :latin1)
                                                      (subseq octets pos (min (+ pos bufsize)
                                                                              (length octets)))))))))

(defmethod reply (connection request (reply reply) (null null))
  (reply connection
         request
         nil
         (format-reply-string reply)))


(defmethod reply (connection request (reply null) (data string))
  (zmq:send (connection-resp-socket connection)
            (make-instance 'zmq:msg
                           :data (format nil
                                         "~A ~A:~A, ~A"
                                         (sender-uuid connection)
                                         (length (connection-id request))
                                         (connection-id request)
                                         data))))

(defmethod reply (connection request reply (file pathname))
  (let ((octets (alexandria:read-file-into-byte-vector file)))
    (setf (wsal:content-type* reply)
          (wsal:mime-type file))
    (reply connection request reply octets)))

(defmethod reply (connection request reply data)
  (reply connection request reply (write-to-string data)))