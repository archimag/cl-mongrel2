;;;; CL-MONGREL2 system definition
;;;;
;;;; This file is part of the cl-mongrel library, released under BSD license.
;;;; See file LICENSE for permissions.
;;;;
;;;; Authors: Vsevolod Dyomkin <vseloved@gmail.com>, Moskvitin Andrey <archimag@gmail.com>

(in-package :asdf)

(defsystem #:cl-mongrel2
  :version '(0 0 2)
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "MIT"
  :description "Lisp interface to mongrel2 web-server"
  :depends-on (#:iterate #:cl-json
               (:version #:zeromq "0.1.1" :above)
               #+nil :uuid)
  :components ((:module "src"
                        :components ((:file "packages")
                                     (:file "request" :depends-on ("packages"))
                                     (:file "reply" :depends-on ("packages"))
                                     (:file "connection" :depends-on ("request" "reply"))))))
;;; end
