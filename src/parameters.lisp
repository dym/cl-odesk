; -*- coding: utf-8; mode: common-lisp; -*-

(in-package :odesk)

(defparameter *library-name* "cl-odesk")

(defparameter *user-agent* (concatenate 'string
                                        *library-name*
                                        " "
                                        *odesk-version-string*))

(defparameter *api-base-url* "https://www.odesk.com/api/")
(defparameter *gds-base-url* "https://www.odesk.com/gds/")
(defparameter *api-auth-url* "https://www.odesk.com/services/api/auth/")

;; Dynamic variable to hold current api instance.
(defparameter *connection* nil)

