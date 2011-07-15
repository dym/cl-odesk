; -*- coding: utf-8; mode: common-lisp; -*-

(in-package :odesk)

(defparameter *library-name* "cl-odesk")
(defparameter *version* "0.3.0")

(defparameter *user-agent* (concatenate 'string
                                        *library-name*
                                        " "
                                        *version*))

(defparameter *api-base-url* "https://www.odesk.com/api/")
(defparameter *api-auth-url* "https://www.odesk.com/services/api/auth/")
(defparameter *oauth-request-token-url* "https://www.odesk.com/api/auth/v1/oauth/token/request")
(defparameter *oauth-access-token-url* "https://www.odesk.com/api/auth/v1/oauth/token/access")

;; Dynamic variable to hold current api instance.
(defparameter *connection* nil)