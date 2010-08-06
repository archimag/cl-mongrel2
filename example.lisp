(in-package :mongrel2)

(defun simple-test ()
  (loop
     (with-connection (conn :sender-uuid "82209006-86FF-4982-B5EA-D1E29E55D483"
                            :sub-addr "tcp://127.0.0.1:9997"
                            :pub-addr "tcp://127.0.0.1:9996")
       (let ((req (recv conn)))
         (reply conn req (format-http-response "abc" 200 :ok ()))))))

;;; to install and run mongrel2 follow instructions in http://mongrel2.org/doc/tip/docs/manual/book.wiki upto
;;; m2sh start -db tests/config.sqlite -host localhost
;;; then from lisp prompt run: (simple-test)
;;; then try several times from shell: curl -i http://localhost:6767/handlertest
;;; expected result:
;;;    HTTP/1.1 200 OK
;;;    Content-length: 4
;;;  
;;;    abc