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
  (setf (wsal:header-out :content-type reply) "text/html"))
  
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

(defun format-reply-string (request reply)
  (with-output-to-string (out)
    (format out
            "~A ~A ~A"
            (or (wsal:header-in :version request)
                "HTTP/1.1")
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
    
    (iter (for (nil . cookie) in (wsal:cookies-out reply))
          (write-string "Set-Cookie: " out)
          (write-string (wsal:stringify-cookie cookie) out)
          (write-string +crlf+ out))
    
    (write-string +crlf+ out)))

(defun maybe-send-close (connection request)
  (when (should-close request)
    (zmq:send (connection-resp-socket connection)
              (make-instance 'zmq:msg
                             :data (format nil
                                           "~A ~A:~A, "
                                           (sender-uuid connection)
                                           (length (connection-id request))
                                           (connection-id request))))))

(defmethod reply (connection request (reply reply) (data string))
  (let ((octets (babel:string-to-octets data :encoding :utf-8)))
    (setf (wsal:header-out :content-length)
          (length octets))
    (reply connection request reply nil)
    (reply connection request nil octets)
    (maybe-send-close connection request)))

(defmethod reply (connection request reply (octets vector))
  (check-type octets (vector (unsigned-byte 8)))
  (setf (wsal:content-length* reply)
        (length octets))
  (reply connection request reply nil)
  (reply connection request nil octets)
  (maybe-send-close connection request))

(defmethod reply (connection request (reply null) (octets vector))
  (check-type octets (vector (unsigned-byte 8)))
  (zmq:send (connection-resp-socket connection)
            (make-instance 'zmq:msg
                           :data (concatenate '(vector (unsigned-byte 8) *)
                                              (babel:string-to-octets (format nil
                                                                              "~A ~A:~A, "
                                                                              (sender-uuid connection)
                                                                              (length (connection-id request))
                                                                              (connection-id request))
                                                                      :encoding :latin1)
                                              octets))))

(defmethod reply (connection request (reply reply) (null null))
  (setf (wsal:header-out :date reply)
        (wsal:rfc-1123-date))
  (reply connection
         request
         nil
         (format-reply-string request reply)))

(defmethod reply (connection request (reply null) (data string))
  (reply connection request nil
         (babel:string-to-octets data :encoding :utf-8)))

(defmethod reply (connection request reply (file pathname))
  (let ((octets (alexandria:read-file-into-byte-vector file)))
    (setf (wsal:content-type* reply)
          (wsal:mime-type file))
    (reply connection request reply octets)))

(defmethod reply (connection request reply data)
  (reply connection request reply (write-to-string data)))
