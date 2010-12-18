;;; CL-MONGREL2 system definition
;;; (c) Vsevolod Dyomkin.  See LICENSE file for permissions

(in-package :asdf)

(defsystem #:cl-mongrel2
  :version '(0 0 2)
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "MIT"
  :description "Lisp interface to mongrel2 web-server"
  :depends-on (:rutils
               :cl-json
               (:version :zeromq "0.1.1" :above)
               #+nil :uuid)
  :components ((:file "mongrel2")))

;;; end
