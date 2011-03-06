; -*- coding: utf-8; mode: common-lisp; -*-

(in-package :odesk)

(defparameter *user-agent* "cl-odesk (Common Lisp oDesk Library) ver. 0.0.0")

(defparameter *url-login* "https://www.odesk.com/login.php")
(defparameter *url-auth* "https://www.odesk.com/services/api/auth")
(defparameter *url-tokens* "https://www.odesk.com/api/auth/v1/keys/tokens.xml")
(defparameter *url-frobs* "https://www.odesk.com/api/auth/v1/keys/frobs.xml")


(defun get-page (url &key (method :get) params)
  (let* ((return-hash (make-hash-table))
         (request (multiple-value-list
                   (http-request url
                                 :want-stream t
                                 :user-agent :firefox
                                 :method method
                                 :parameters params)))
         (status-code (elt request 1))
         (headers (elt request 2))
         (stream (elt request 4))
         (reason (elt request 6))
         (text (iter
                 (for line next (read-line stream nil 'EOF))
                 (until (eq line 'EOF))
                 (collect (remove #\Return line)))))
    (close stream)
    ; Pack return values to the hash table
    (setf (gethash 'code return-hash) status-code)
    (setf (gethash 'headers return-hash) headers)
    (setf (gethash 'reason return-hash) reason)
    (setf (gethash 'text return-hash) text)
    return-hash))