; -*- coding: utf-8; mode: common-lisp; -*-

(in-package :cl-user)

(defpackage :odesk
  (:use :common-lisp
        :md5
        :drakma)
  (:export :api-json
           :api-xml
           :urlencode))
