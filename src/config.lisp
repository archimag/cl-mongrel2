;;;; config.lisp
;;;;
;;;; This file is part of the cl-mongrel library, released under BSD license.
;;;; See file LICENSE for permissions.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:mongrel2)

(defclass server ()
  ((uuid :initarg :uuid :type string :reader uuid)
   (access-log :initarg :access-log :type string :reader access-log)
   (error-log :initarg :error-log :type string :reader error-log)
   (chroot :initarg  :chroot :type string :reader chroot)
   (default-host :initarg :default-host :type string :reader default-host)
   (name :initarg :name :type string :reader :name)
   (pid-file :initarg :pid-file :type string :reader pid-file)
   (port :initarg :port :type integer :reader port)))

(defclass host ()
  ((server :initarg :server :initform nil :type server :reader server)
   (maintenance :initarg :maintenance :initform 0 :type boolean :reader maintenance)
   (name :initarg :name :type string :reader :name)
   (matching :initarg :matching :initform "" :type string :reader matching)))

(defclass proxy ()
  ((addr :initform :addr :type string)
   (port :initform :port :type integer)))

(defclass dir ()
  ((base :initarg :base :type string)
   (index-file :initarg :index-file :type string)
   (default-ctype :initarg default-ctype :initform "text/plain" :type string)))

(defclass target () ())

(defclass handler ()
  ((send-spec :initarg :send-spec :initform "" :type string)
   (send-ident :initarg :send-ident :initform "" :type string)
   (recv-spec :initarg :recv-spec :initform "" :type string)
   (recv-ident :initarg :recv-ident :initform "" :type string)
   (raw-payload :initform 0 :type integer)))
  
(defclass route ()
  ((path :initarg :path :initform "")
   (reversed :initarg :reversed :initform 0 :type boolean)
   (host :initarg :host :initform nil :type host)
   (target :initarg :target :initform nil :type target )
   (target-type :initarg :target-id :initform "handler" :type string)))

(defclass mimetype ()
  ((mimetype :initform :mimetype :type string)
   (extension :initform :extension :type string)))

(defclass setting ()
  ((key :initform :key :type string)
   (value :initform :value :type string)))

(defparameter *classes* '(server host proxy dir handler route mimetype setting))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; slot-sql-type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric slot-sql-type (slot) )

(defmethod slot-sql-type ((slot closer-mop:slot-definition))
  (slot-sql-type (closer-mop:slot-definition-type slot)))

(defmethod slot-sql-type ((slot symbol))
  (slot-sql-type (find-class slot)))

(defmethod slot-sql-type ((slot (eql 'string)))
  "TEXT")

(defmethod slot-sql-type ((slot (eql 'boolean)))
  "BOOLEAN")

(defmethod slot-sql-type ((class closer-mop:built-in-class))
  (symbol-name (class-name class)))

(defmethod slot-sql-type ((class closer-mop:standard-class))
  "INTEGER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; slot-sql-name
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric slot-sql-name (slot) )

(defmethod slot-sql-name ((slot closer-mop:slot-definition))
  (let ((name (closer-mop:slot-definition-name slot))
        (type (ignore-errors (find-class (closer-mop:slot-definition-type slot)))))
    (if (and type (typep type 'closer-mop:standard-class))
        (format nil "~A_id" (class-sql-table-name type))
        (slot-sql-name name))))

(defmethod slot-sql-name ((slot symbol))
  (ppcre:regex-replace-all  "-" (string-downcase (symbol-name slot)) "_"))

(defmethod slot-sql-name ((slot (eql 'target)))
  "target_id")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; table-name
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric class-sql-table-name (class) )

(defmethod class-sql-table-name ((class closer-mop:class))
  (class-sql-table-name (class-name class)))
  
(defmethod class-sql-table-name ((class-symbol symbol))
  (string-downcase (symbol-name class-symbol)))

(defmethod class-sql-table-name ((class-symbol (eql 'dir)))
  "directory")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-table-sql (class))

(defmethod create-table-sql ((class-symbol symbol))
  (create-table-sql (find-class class-symbol)))

(defmethod create-table-sql ((class closer-mop:class))
  (closer-mop:finalize-inheritance class)
  (format nil
          "CREATE TABLE ~A (id INTEGER PRIMARY KEY, ~{~A ~A~^, ~})"
          (class-sql-table-name class)
          (iter (for slot in (closer-mop:class-slots class))
                (collect (slot-sql-name slot))
                (collect (slot-sql-type slot)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; slot-value-sql
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric slot-value-sql (slot value))

(defmethod slot-value-sql (slot (value string))
  (format nil "'~A'" value))

(defmethod slot-value-sql (slot value)
  (if (typep (class-of value) 'closer-mop:standard-class)
      1
      value))
  
(defmethod slot-value-sql (slot (value null))
  "NULL")
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert-object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric insert-object-sql (obj)
  )

(defmethod insert-object-sql (obj)
  (let ((type (class-of obj)))
    (format nil
            "INSERT INTO ~A (~{~A~^,~}) VALUES (~{~A~^,~})"
            (class-sql-table-name type)
            (iter (for slot in (closer-mop:class-slots type))
                  (collect (slot-sql-name slot)))
            (iter (for slot in (closer-mop:class-slots type))
                  (collect (slot-value-sql slot
                                           (closer-mop:slot-value-using-class type obj slot)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; create-test-config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clear-db (db)
  (iter (for class in *classes*)
        (sqlite:execute-non-query db
                                  (format nil
                                          "DELETE FROM ~A"
                                          (class-sql-table-name class)))))

(defun create-tables (db)
  (iter (for class in *classes*)
        (when (= (sqlite:execute-single db
                                        "select count(*) from sqlite_master where type='table' and name=?"
                                        (class-sql-table-name class))
                 0)
          (sqlite:execute-non-query db (create-table-sql class)))))

(defun create-test-config (dir)
  (ensure-directories-exist dir)
  (ensure-directories-exist (merge-pathnames "run/" dir))
  (ensure-directories-exist (merge-pathnames "logs/" dir))
    
  (sqlite:with-open-database (db (merge-pathnames "config.sqlite" dir))
    (create-tables db)
    (clear-db db)
    
    (let* ((server (make-instance 'server
                                  :uuid "82209006-86FF-4982-B5EA-D1E29E55D483"
                                  :access-log "/logs/access.log"
                                  :error-log "/logs/error.log"
                                  :chroot (cffi-sys:native-namestring dir)
                                  :default-host "localhost"
                                  :name "test"
                                  :pid-file "/run/mongrel2.pid"
                                  :port 6767))
           (host (make-instance 'host
                                :server server
                                :name "localhost"))
           (handler (make-instance 'handler
                                   :send-spec "tcp://127.0.0.1:9997"
                                   :send-ident "34f9ceee-cd52-4b7f-b197-88bf2f0ec378"
                                   :recv-spec "tcp://127.0.0.1:9996"
                                   :recv-ident ""))
           (route (make-instance 'route
                                 :path "/"
                                 :target handler
                                 :host host)))
      (iter (for obj in (list server host handler route))
            (sqlite:execute-non-query db (insert-object-sql obj)))
      )))
