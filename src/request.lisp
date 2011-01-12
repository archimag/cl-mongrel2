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
   (headers :reader request-headers)
   (body  :reader request-body)
   (data :accessor request-data :initform ())))


(defun find-in-memory (thing ptr start end &aux (byte (char-code thing)))
  (iter (for pos from start below end)
        (finding pos
                 such-that (= (cffi:mem-ref ptr :char pos)
                              byte))))

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
    (let* ((end (find-in-memory #\: raw curpos size))
           (len (parse-integer (cffi:foreign-string-to-lisp raw
                                                            :offset curpos
                                                            :count (- end curpos))))
           (json:*json-identifier-name-to-lisp* #'json:simplified-camel-case-to-lisp))
      (setf curpos (1+ end))
      (setf (slot-value request 'headers)
            (json:decode-json-from-string (cffi:foreign-string-to-lisp raw
                                                                       :offset curpos
                                                                       :count len))))))
