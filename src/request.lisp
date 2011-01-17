;;;; request.lisp
;;;;
;;;; This file is part of the cl-mongrel library, released under BSD license.
;;;; See file LICENSE for permissions.
;;;;
;;;; Authors: Vsevolod Dyomkin <vseloved@gmail.com>, Moskvitin Andrey <archimag@gmail.com>

(in-package #:mongrel2)

(defclass request ()
  ((sender-uuid :reader sender-uuid)
   (connection-id :reader connection-id)
   (path :reader request-path)
   (headers :reader headers-in)
   (body-offset :reader request-body-offset)
   (body-lenght  :reader request-body-length)
   (msg :initarg :msg :reader request-msg)))

(defun find-in-memory (thing ptr start end &aux (byte (char-code thing)))
  (iter (for pos from start below end)
        (finding pos
                 such-that (= (cffi:mem-ref ptr :char pos)
                              byte))))

(defun parse-netstring-header (ptr start end)
  (let ((pos (find-in-memory #\: ptr start end)))
    (if pos
        (values pos
                (parse-integer (cffi:foreign-string-to-lisp ptr
                                                            :offset start
                                                            :count (- pos start)))))))

(defmethod shared-initialize :after ((request request) slot-names &key msg &allow-other-keys)
  (let ((curpos 0)
        (size (zmq:msg-size msg))
        (raw (zmq:msg-data-as-is msg)))
    (iter (for slot in '(sender-uuid connection-id path))
          (for end = (find-in-memory #\Space raw curpos size))
          (setf (slot-value request slot)
                (cffi:foreign-string-to-lisp raw
                                             :offset curpos
                                             :count (- end curpos)))
          (setf curpos (1+ end)))
    (multiple-value-bind (end len) (parse-netstring-header raw curpos size)
      (setf curpos (1+ end))
      (setf (slot-value request 'headers)
            (let ((json:*json-identifier-name-to-lisp* #'json:simplified-camel-case-to-lisp))
              (json:decode-json-from-string 
               (cffi:foreign-string-to-lisp raw
                                            :offset curpos
                                            :count len))))
      (incf curpos (1+ len)))
    (multiple-value-bind (end len) (parse-netstring-header raw curpos size)
      (setf (slot-value request 'body-offset)
            (1+ end))
      (setf (slot-value request 'body-lenght)
            len))))

(defun raw-post-data (request &key (encoding :utf-8))
  (let ((raw (zmq:msg-data-as-is (request-msg request)))
        (len (request-body-length request))
        (offset (request-body-offset request)))
    (if (eql encoding :binary)
        (let ((arr (cffi:make-shareable-byte-vector len)))
          (cffi:with-pointer-to-vector-data (ptr arr) 
            (zmq::memcpy ptr 
                         (cffi:make-pointer (+ (cffi:pointer-address raw)
                                               offset))
                         len))
          arr)
        (cffi:foreign-string-to-lisp raw
                                     :offset offset
                                     :count len
                                     :encoding encoding))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recv
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun recv (conn)
  (let ((msg (make-instance 'zmq:msg)))
    (zmq:recv (connection-req-socket conn) msg)
    (make-instance 'request :msg msg)))
