; -*- coding: utf-8; mode: common-lisp; -*-

(in-package :odesk)

(defparameter *library-name* "cl-odesk")
(defparameter *version* "0.0.0")

(defparameter *user-agent* (concatenate 'string
                                        *library-name*
                                        " "
                                        *version*))

(defparameter *api-base-url* "https://www.odesk.com/api/")