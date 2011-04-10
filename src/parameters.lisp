; -*- coding: utf-8; mode: common-lisp; -*-

(in-package :odesk)

(defparameter *library-name* "cl-odesk")
(defparameter *version* "1.0.0-pre")

(defparameter *user-agent* (concatenate 'string
                                        *library-name*
                                        " "
                                        *version*))

(defparameter *api-base-url* "https://www.odesk.com/api/")
(defparameter *api-auth-url* "https://www.odesk.com/services/api/auth/")

;; Dynamic variable to hold current api instance.
(defparameter *connection* nil)