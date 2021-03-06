; -*- coding: utf-8; mode: common-lisp; -*-

(in-package :odesk)

(defun get-page (url &key (method :get) parameters)
  ;(format t "URL: ~a~%parameters: ~a~%" url parameters)
  (let* ((return-hash (make-hash-table))
         (request (multiple-value-list
                   (http-request url
                                 :want-stream t
                                 :user-agent *user-agent*
                                 :method method
                                 :parameters parameters)))
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

;; Format url like python .format method
;;   (i.e.: "{username}/{tray}".format(username, tray)
(defun format-url (url &key from-subs to-subs)
  (let ((regex (copy-seq url)))
    (iter
      (for sub in from-subs)
      (for i from 0)
      (setf regex (cl-ppcre:regex-replace (concatenate 'string
                                                       "{"
                                                       (string-downcase sub)
                                                       "}")
                                          regex
                                          (elt to-subs i))))
    regex))