(defpackage #:mongrel2
  (:use #:cl #:rutils.usr))

(in-package #:mongrel2)

;; util

(defun fmt-crlf (stream format &rest args)
  (macrolet ((body (stream)
               `(progn (apply #'format ,stream format args)
                       (write-char #\return ,stream)
                       (write-char #\linefeed ,stream)
                       (values))))
    (if stream (body stream)
        (with-output-to-string (out)
          (body out)))))

(defun fmt-mongrel (conn format &rest args)
  (zmq:send (connection-resp-socket conn)
            (make-instance 'zmq:msg
                           :data (apply #'fmt (strcat_ "~A" format)
                                        (sender-uuid conn)
                                        args))))

(defun parse-netstring (str)
  (let* ((length-end (position #\: str))
         (length (parse-integer (subseq str 0 length-end)))
         (rest (subseq str (1+ length-end))))
    (assert (char= (char rest length) #\,)
            () "Netstring did not end in ','")
    (values (subseq rest 0 length)
            (subseq rest (1+ length)))))

#+mutest
(deftest parse-netstring ()
  '("3:123,")     '("123" "")
  '("3:123,abc")  '("123" "abc"))

;; classes

(defclass request ()
  ((sender-uuid :initarg :sender-uuid :type string
                :reader sender-uuid
                :initform (error "Should provide sender UUID"))
   (connection-id :initarg :connection-id :type integer
                  :reader connection-id
                  :initform (error "Should provide connection ID"))
   (path :initarg :path :type string
         :accessor request-path :initform "/")
   (headers :initarg :headers :type list
            :accessor request-headers :initform ())
   (body :initarg :body :type string
         :accessor request-body :initform "")
   (data :initarg :data :type list
         :accessor request-data :initform ())))

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


;; wrappers

(defmacro with-connection ((conn &rest init-args
                                 &key sender-uuid sub-addr pub-addr)
                           &body body)
  (with-gensyms (ctx req-socket resp-socket)
    `(let ((,conn (make-instance 'connection
                                 :sender-uuid ,sender-uuid
                                 ;;(print-object (uuid:make-v1-uuid) nil)
                                 ,@init-args)))
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

(defun parse-request (msg)
  (bind (((sender-uuid connection-id path &rest rest) (split-sequence #\Space
                                                                      msg))
         (raw-headers rest (parse-netstring (reduce #'strcat_ rest)))
         (headers (let ((json:*json-identifier-name-to-lisp*
                         #'json:simplified-camel-case-to-lisp))
                    (json:decode-json-from-string raw-headers)))
         (body (parse-netstring rest)))
    (make-instance 'request
                   :sender-uuid sender-uuid
                   :connection-id (parse-integer connection-id)
                   :path path
                   :headers headers
                   :body body
                   :data (when (string= (cdr (assoc :method  headers)) "JSON")
                           (flatten (json:decode-json-from-string body))))))

(defmethod disconnected? (request)
  (and (string= (getf (request-headers request) :method) "JSON")
       (string= (getf (request-data request) :type) "disconnect")))

(defun format-http-response (body code status headers)
  (with-output-to-string (resp-stream)
    (fmt-crlf resp-stream "HTTP/1.1 ~A ~A" code status)
    (fmt-crlf resp-stream "Content-Length: ~A" (length body))
    (loop :for (k v) :on headers :by #'cddr :do
       (fmt-crlf resp-stream "~:(~A~): ~A" k v))
    (fmt-crlf resp-stream "")
    (format resp-stream "~A" body)))

(defmethod recv ((conn connection))
  (let ((msg (make-instance 'zmq:msg)))
    (zmq:recv (connection-req-socket conn) msg)
    (parse-request (zmq:msg-data-as-string msg))))

(defmethod send ((conn connection) conn-id msg)
  (fmt-mongrel conn "~A:~A, ~A" (length (princ-to-string conn-id)) conn-id msg))

(defmethod reply ((conn connection) req msg)
  (send conn (connection-id req) msg))

(defmethod deliver ((conn connection) conn-ids msg)
  (fmt-mongrel conn "~A:~{~A~}, ~A"
               (reduce #'+ (mapcar (lambda (str)
                                     (length (princ-to-string str)))
                                   conn-ids)
                       :initial-value (1- (length conn-ids)))
               (fmt "~A " conn-ids) msg))

;;; end

