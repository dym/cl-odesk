; -*- coding: utf-8; mode: common-lisp; -*-

(in-package :odesk)

(defconstant user-agent "cl-odesk (Common Lisp oDesk Library)")

(defconstant url-login "https://www.odesk.com/login.php")
(defconstant url-auth "https://www.odesk.com/services/api/auth")
(defconstant url-tokens "https://www.odesk.com/api/auth/v1/keys/tokens.xml")
(defconstant url-frobs "https://www.odesk.com/api/auth/v1/keys/frobs.xml")


(defclass api ()
  ((public-key
    :initarg :public-key
    :initform (error "Must supply public key.")
    :accessor public-key
    :documentation "Public key")
   (secret-key
    :initarg :secret-key
    :initform (error "Must supply secret key.")
    :accessor secret-key
    :documentation "Secret key")
   (api-token
    :initarg :api-token
    :initform (error "Must supply api token.")
    :accessor api-token
    :documentation "Api token")))

(defclass api-json (api)
  ((data-format
    :initform "json"
    :accessor data-format
    :documentation "Data Format")))

(defclass api-xml (api)
  ((data-format
    :initform "xml"
    :accessor data-format
    :documentation "Data Format")))

(defgeneric signurl (api params)
  (:documentation "Sign url params."))

(defmethod signurl ((api api) params)
  (let* ((copy-params (copy-tree params))
         (sorted-params (sort copy-params
                              #'string<
                              :key #'car))
         (flaten-params (format nil
                                "~(~{~2,'0X~}~)"
                                (map 'list
                                     #'(lambda (lst)
                                         (concatenate 'string
                                                      (car lst)
                                                      (cdr lst)))
                                     sorted-params))))
    (with-accessors ((secret-key secret-key)) api
      (format nil "~(~{~2,'0X~}~)"
              (map 'list #'identity
                   (md5:md5sum-sequence (concatenate 'string
                                                     secret-key
                                                     flaten-params)))))))

(defgeneric urlencode (api params)
  (:documentation "Encode url parameters."))

(defmethod urlencode ((api api) params)
  (with-accessors ((public-key public-key)
                   (secret-key secret-key)
                   (api-token api-token)) api
    (append (list (cons "api_key" public-key)
                  (cons "api_token" api-token)
                  (cons "api_sig" (signurl api params)))
            params)))