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
  :components ((:file "packages")
               (:file "odesk" :depends-on ("packages")))
  :depends-on (#:md5 #:drakma))