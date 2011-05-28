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

(defgeneric sign-url (api &key parameters)
  (:documentation "Sign url parameters."))

(defmethod sign-url ((api api) &key parameters)
  (let* ((copy-parameters (copy-tree parameters))
         (sorted-parameters (sort copy-parameters
                                  #'string<
                                  :key #'car))
         (flaten-parameters (format nil
                                    "~(~{~2,'0X~}~)"
                                    (map 'list
                                         #'(lambda (lst)
                                             (concatenate 'string
                                                          (car lst)
                                                          (cdr lst)))
                                         sorted-parameters))))
    (with-accessors ((secret-key secret-key)) api
      (format nil "~(~{~2,'0X~}~)"
              (map 'list #'identity
                   (md5:md5sum-sequence (concatenate 'string
                                                     secret-key
                                                     flaten-parameters)))))))

(defgeneric url-encode (api &key parameters)
  (:documentation "Encode url parameters."))

(defmethod url-encode ((api api) &key parameters)
  (with-accessors ((public-key public-key)
                   (secret-key secret-key)
                   (api-token api-token)) api
    (push (cons "api_key" public-key) parameters)
    (if api-token
        (push (cons "api_token" api-token) parameters))
    (push (cons "api_sig" (sign-url api :parameters parameters)) parameters)
    parameters))

(defun auth-url (&key (connection *connection*) frob)
  "Return authentication url to be used in browser."
  (let ((parameters (if frob
                        (list (cons "frob" frob)))))
    (format nil
            "~a?~(~{~2,'0X~^&~}~)"
            *api-auth-url*
            (map 'list
                 #'(lambda (lst)
                     (concatenate 'string
                                  (car lst)
                                  "="
                                  (cdr lst)))
                 (url-encode connection :parameters parameters)))))

(defgeneric url-read (api url &key parameters method)
  (:documentation "Return parsed object."))

(defmethod url-read ((api api) url &key parameters (method :get))
  (with-slots ((data-format data-format)) api
    (let ((get-url (concatenate 'string
                                url
                                "."
                                data-format))
          (copy-parameters (copy-tree parameters)))
      (if (or (eql method :put)
              (eql method :delete))
          (setf copy-parameters
                (append (list (cons "http_method"
                                    (string-downcase method))))))
      (parse-page api (get-page get-url
                                :method method
                                :parameters (url-encode api :parameters copy-parameters))))))

(defgeneric parse-page (api page)
  (:documentation "Parse fetched page."))

(defmethod parse-page ((api api) page)
  (let ((code (gethash 'code page))
        (text (gethash 'text page)))
    (if (eql code 200) ; that means HTTP_OK
        text
        nil)))

(defmacro def-req (request (&key url (method :get) (version nil)
                                 (sub-url nil)) docstring)
  (let ((area-url (first (split-sequence #\/ (string-downcase request))))
        (from-subs (mapcar #'string-downcase sub-url)))
    (alexandria:with-gensyms (base-url api-version url-version ready-url full-url)
      `(progn
         (defun ,request (&key (connection *connection*) parameters ,@sub-url)
           ,docstring
           (with-slots ((,base-url base-url)
                        (,api-version api-version)) connection
             (let* ((,url-version (or ,version ,api-version))
                    (,ready-url (if (every #'stringp (list ,@sub-url))
                                    (format-url ,url
                                                :from-subs ',from-subs
                                                :to-subs (list ,@sub-url))
                                    ,url))
                    (,full-url (format nil
                                       "~a~a/v~a/~a"
                                       ,base-url
                                       ,area-url
                                       ,url-version
                                       ,ready-url)))
               (url-read connection ,full-url :parameters parameters :method ,method))))
         (export ',request :odesk)))))

; Example: (with-odesk (:connection con :public-key "PK" :secret-key "SK") (print con))
(defmacro with-odesk ((&key (connection '*connection*)
                            (format :json)
                            public-key
                            secret-key
                            api-token)
                      &body body)
  (let ((api-class (read-from-string (concatenate 'string
                                                  "'odesk:api-"
                                                  (string-downcase format)))))
    `(progn
       (let ((,connection (make-instance ,api-class
                                         :public-key ,public-key
                                         :secret-key ,secret-key
                                         :api-token ,api-token)))
         ,@body))))