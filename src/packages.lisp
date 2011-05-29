; -*- coding: utf-8; mode: common-lisp; -*-

(in-package :cl-user)

(defpackage :odesk
  (:use :common-lisp
        :iter
        :md5
        :split-sequence
        :drakma)
  (:export :api-json
           :api-xml
           :auth-url
           :with-odesk
           :connect-odesk
           :*connection*))
