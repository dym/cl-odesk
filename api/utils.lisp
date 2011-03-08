; -*- coding: utf-8; mode: common-lisp; -*-

(in-package :odesk)

(defun get-page (url &key (method :get) params)
  ;(format t "URL: ~a~%params: ~a~%" url params)
  (let* ((return-hash (make-hash-table))
         (request (multiple-value-list
                   (http-request url
                                 :want-stream t
                                 :user-agent *user-agent*
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