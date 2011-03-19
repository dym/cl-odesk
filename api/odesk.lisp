; -*- coding: utf-8; mode: common-lisp; -*-

(in-package :odesk)

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
    :initform nil
    :accessor api-token
    :documentation "Api token")
   (api-version
    :initarg :api-version
    :initform 1
    :accessor api-version
    :documentation "Major version of the API.")
   (base-url
    :initarg :base-url
    :initform *api-base-url*
    :accessor base-url
    :documentation "Base url for API requests.")))

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

(defgeneric sign-url (api params)
  (:documentation "Sign url params."))

(defmethod sign-url ((api api) params)
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

(defgeneric url-encode (api params)
  (:documentation "Encode url parameters."))

(defmethod url-encode ((api api) params)
  (with-accessors ((public-key public-key)
                   (secret-key secret-key)
                   (api-token api-token)) api
    (push (cons "api_key" public-key) params)
    (if api-token
        (push (cons "api_token" api-token) params))
    (push (cons "api_sig" (sign-url api params)) params)
    params))

(defgeneric url-read (api url params &key method)
  (:documentation "Return parsed object."))

(defmethod url-read ((api api) url params &key (method :get))
  (with-slots ((data-format data-format)) api
    (let ((get-url (concatenate 'string
                                url
                                "."
                                data-format))
          (copy-params (copy-tree params)))
      (if (or (eql method :put)
              (eql method :delete))
          (setf copy-params
                (append (list (cons "http_method"
                                    (string-downcase method))))))
      (parse-page api (get-page get-url
                                :method method
                                :params (url-encode api copy-params))))))

(defgeneric parse-page (api page)
  (:documentation "Parse fetched page."))

(defmethod parse-page ((api api) page)
  (let ((code (gethash 'code page))
        (text (gethash 'text page)))
    (if (eql code 200) ; that measn HTTP_OK
        text
        nil)))

(defmacro def-req (request (&key url (method :get) (version nil) (params nil)) docstring)
  `(progn
     (defgeneric ,request (api &key params)
       (:documentation ,docstring))
     (defmethod ,request ((api api) &key params)
       (with-slots ((base-url base-url)
                    (api-version api-version)) api
         (let* ((request-str (string-downcase ',request))
                (request-split (split-sequence #\/ request-str))
                (version (or ',version api-version))
                (full-url (format nil
                                  "~a~a/v~a/~a"
                                  base-url
                                  (first request-split) ; area part of request
                                  version
                                  ,url)))
           (url-read api full-url ,params :method ,method))))
     (export ',request :odesk)))