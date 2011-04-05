; -*- coding: utf-8; mode: common-lisp; -*-

(defpackage :odesk-api-asd
  (:use :cl
        :asdf))

(in-package :odesk-api-asd)

(defsystem odesk
  :name "Common Lisp oDesk Library"
  :version "0.0.0"
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
  :depends-on (#:iterate #:md5 #:drakma #:split-sequence #:cl-ppcre))