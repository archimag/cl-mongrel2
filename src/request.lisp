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
   (headers :reader wsal:headers-in)
   (body-offset :reader request-body-offset)
   (body-lenght  :reader request-body-length)
   (msg :initarg :msg :reader request-msg)

   (get-parameters :initform nil :reader wsal:get-parameters)
   (cookies-in :initform nil :reader wsal:cookies-in)
   (post-parameters :initform nil :reader wsal:post-parameters)))

(defun recv (conn)
  (let ((msg (make-instance 'zmq:msg)))
    (zmq:recv (connection-req-socket conn) msg)
    (make-instance 'request :msg msg)))

(defun is-disconnect (request)
  (string= (wsal:request-method request)
           "JSON"))

(defun should-close (request)
  (or (string= (wsal:header-in :connection request)
               "close")
      (string= (wsal:header-in :version request)
               "HTTP/1.0")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
            len))

    (setf (slot-value request 'get-parameters)
          (wsal:form-url-encoded-list-to-alist
           (ppcre:split "&" (wsal:query-string request))))

    (setf (slot-value request 'cookies-in)
          (wsal:form-url-encoded-list-to-alist
           (ppcre:split "\\s*[,;]\\s*" (cdr (assoc :cookie (wsal:headers-in request)
                                                   :test #'eq)))
           :utf-8))

    (let* ((content-type (wsal:header-in :content-type request))
           (type (if content-type
                     (second (wsal.rfc2388:parse-header content-type :value)))))
      (cond
        ((string= type "application/x-www-form-urlencoded")
         (setf (slot-value request 'post-parameters)
               (wsal:form-url-encoded-list-to-alist
                (ppcre:split "&" (wsal:raw-post-data request :encoding :latin1))
                :utf-8)))
        
        ((string= type "multipart/form-data")
         (setf (slot-value request 'post-parameters)
               (wsal:parse-multipart-form-data request :utf-8)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WSAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod wsal:query-string ((request request))
  (wsal:header-in :query request))

(defmethod wsal:script-name ((request request))
  (wsal:header-in :path request))

(defmethod wsal:request-method ((request request))
  (intern (wsal:header-in :method request) :keyword))

(defmethod wsal:request-uri ((request request))
  (wsal:header-in :uri request))

(defmethod wsal:server-protocol ((request request))
  (wsal:make-keyword (wsal:header-in :version request) nil))

(defmethod wsal:remote-address ((request request))
  nil)

(defmethod wsal:remote-port ((request request))
  nil)

(defmethod wsal:raw-post-data ((request request) &key encoding force-text force-binary)
  (let ((raw (zmq:msg-data-as-is (request-msg request)))
        (len (request-body-length request))
        (offset (request-body-offset request)))
    (cond
      (force-binary
       (let ((arr (cffi:make-shareable-byte-vector len)))
         (cffi:with-pointer-to-vector-data (ptr arr) 
           (zmq::memcpy ptr 
                        (cffi:make-pointer (+ (cffi:pointer-address raw)
                                              offset))
                        len))
         arr))
      ((or force-text encoding)
       (cffi:foreign-string-to-lisp raw
                                    :offset offset
                                    :count len
                                    :encoding (or encoding :utf-8)))
      (t (cffi:foreign-string-to-lisp raw
                                    :offset offset
                                    :count len
                                    :encoding :utf-8)))))
  