; -*- coding: utf-8; mode: common-lisp; -*-

(in-package :cl-user)

(defpackage :odesk
  (:use :common-lisp
        :iter
        :md5
        :split-sequence
        :drakma)
  (:import-from :odesk-api-asd :*odesk-version-string*)
  (:import-from :drakma :url-encode)
  (:export :api
           :auth-url
           :url-encode
           :with-odesk
           :connect-odesk
           :*connection*))
