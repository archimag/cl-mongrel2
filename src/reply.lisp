;;;; util.lisp
;;;;
;;;; This file is part of the cl-mongrel library, released under BSD license.
;;;; See file LICENSE for permissions.
;;;;
;;;; Authors: Vsevolod Dyomkin <vseloved@gmail.com>, Moskvitin Andrey <archimag@gmail.com>

(in-package #:mongrel2)

(defun fmt-crlf (stream format &rest args)
  (macrolet ((body (stream)
               `(progn (apply #'format ,stream format args)
                       (write-char #\return ,stream)
                       (write-char #\linefeed ,stream)
                       (values))))
    (if stream (body stream)
        (with-output-to-string (out)
          (body out)))))

(defun fmt (format-string &rest args)
  (apply #'format nil format-string args))

(defun fmt-mongrel (conn format &rest args)
  (zmq:send (connection-resp-socket conn)
            (make-instance 'zmq:msg
                           :data (apply #'fmt
                                        (concatenate 'string "~A " format)
                                        (sender-uuid conn)
                                        args))))

(defun format-http-response (body code status headers)
  (with-output-to-string (resp-stream)
    (fmt-crlf resp-stream "HTTP/1.1 ~A ~A" code status)
    (fmt-crlf resp-stream "Content-Length: ~A" (length body))
    (loop :for (k v) :on headers :by #'cddr :do
       (fmt-crlf resp-stream "~:(~A~): ~A" k v))
    (fmt-crlf resp-stream "")
    (format resp-stream "~A" body)))