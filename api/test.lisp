; -*- coding: utf-8; mode: common-lisp; -*-

(in-package :cl-user)

(load "odesk.asd")
(require :odesk)

(defparameter *test-connection*
  (make-instance 'odesk:api-json
                 :public-key "PUBLIC"
                 :secret-key "SECRET"
                 :api-token "TOKEN"))
