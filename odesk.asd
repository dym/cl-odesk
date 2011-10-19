; -*- coding: utf-8; mode: common-lisp; -*-

(defpackage :odesk-api-asd
  (:use :cl
        :asdf))

(in-package :odesk-api-asd)

(defvar *odesk-version-string* "0.3.1"
  "cl-odesk version number as a string.")

;; we export its name so we can import it later
(export '*odesk-version-string*)


(defsystem #:odesk
  :name "Common Lisp oDesk Library"
  :version #.*odesk-version-string*
  :author "Dmitriy Budashny <dmitriy.budashny@gmail.com>"
  :license "BSD"
  :components
  ((:module "src"
            :components
            ((:file "packages")
             (:file "parameters" :depends-on ("packages"))
             (:file "utils" :depends-on ("parameters"))
             (:file "odesk" :depends-on ("utils"))
             (:file "requests" :depends-on ("odesk")))))
  :depends-on (#:iterate #:md5 #:drakma #:split-sequence
                         #:cl-ppcre #:alexandria))