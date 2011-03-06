; -*- coding: utf-8; mode: common-lisp; -*-

(in-package :cl-user)

(defpackage :odesk
  (:use :common-lisp
        :iter
        :md5
        :drakma)
  (:export :api-json
           :api-xml
           :url-encode
           :url-read
           :get-page))
