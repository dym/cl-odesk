; -*- coding: utf-8; mode: common-lisp; -*-

(in-package :odesk)

(defparameter *version* "0.0.0")
(defparameter *user-agent* (concatenate 'string
                                        "cl-odesk "
                                        *version*))