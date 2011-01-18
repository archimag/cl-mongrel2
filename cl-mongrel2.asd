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
  :depends-on (#:iterate #:cl-json #:zeromq #:sqlite #:closer-mop #:cl-ppcre)
  :components ((:module "src"
                        :components ((:file "packages")
                                     (:file "connection" :depends-on ("packages"))
                                     (:file "request" :depends-on ("connection"))
                                     (:file "reply" :depends-on ("connection"))
                                     (:file "config" :depends-on ("packages"))))))
;;; end
