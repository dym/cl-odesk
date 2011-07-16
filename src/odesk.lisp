; -*- coding: utf-8; mode: common-lisp; -*-

(in-package :odesk)

(defclass api ()
  ((data-format
    :initform "json"
    :accessor data-format
    :documentation "Data Format")
   (public-key
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
   (api-url
    :initarg :api-url
    :initform *api-base-url*
    :accessor api-url
    :documentation "Base url for API requests.")
   (gds-url
    :initarg :gds-url
    :initform *gds-base-url*
    :accessor gds-url
    :documentation "Base GDS url for API requests.")))

(defgeneric sign-url (api &key parameters)
  (:documentation "Sign url parameters."))

(defmethod sign-url ((api api) &key parameters)
  (let* ((copy-parameters (copy-tree parameters))
         (sorted-parameters (sort copy-parameters
                                  #'string<
                                  :key #'car))
         (flaten-parameters (format nil
                                    "~{~2,'0X~}"
                                    (map 'list
                                         #'(lambda (lst)
                                             (format nil "~a~a"
                                                     (car lst)
                                                     (cdr lst)))
                                         sorted-parameters))))
    (with-accessors ((secret-key secret-key)) api
      (format nil "~(~{~2,'0X~}~)"
              (map 'list #'identity
                   (md5:md5sum-sequence (concatenate 'string
                                                     secret-key
                                                     flaten-parameters)))))))

(defgeneric encode-url (api &key parameters)
  (:documentation "Encode url parameters."))

(defmethod encode-url ((api api) &key parameters)
  (with-accessors ((public-key public-key)
                   (secret-key secret-key)
                   (api-token api-token)) api
    (push (cons "api_key" public-key) parameters)
    (if api-token
        (push (cons "api_token" api-token) parameters))
    (push (cons "api_sig" (sign-url api :parameters parameters)) parameters)
    parameters))

(defgeneric build-auth-url (api &key frob)
  (:documentation "Return authentication url to be used in browser.."))

(defmethod build-auth-url ((api api) &key frob)
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
                 (encode-url api :parameters parameters)))))

(defun auth-url (&key (connection *connection*) frob)
  "Return authentication url to be used in browser."
  (build-auth-url connection :frob frob))

(defgeneric url-read (api url &key parameters method)
  (:documentation "Return parsed object."))

(defmethod url-read ((api api) url &key parameters (method :get) (request-type :api))
  (with-slots ((data-format data-format)) api
    (let* ((get-url (cond ((equal request-type :gds) url)
                          (t (concatenate 'string
                                          url
                                          "."
                                          data-format))))
           (copy-parameters (copy-tree parameters))
           (copy-parameters (cond ((equal request-type :gds)
                                   (append (list (cons "tqx"
                                                       (format nil "out:~a" (string-downcase data-format))))
                                           copy-parameters))
                                  (t copy-parameters))))
      (if (or (eql method :put)
              (eql method :delete))
          (setf copy-parameters
                (append (list (cons "http_method"
                                    (string-downcase method))))))
      (parse-page api (get-page get-url
                                :method method
                                :parameters (encode-url api :parameters copy-parameters))))))

(defgeneric parse-page (api page)
  (:documentation "Parse fetched page."))

(defmethod parse-page ((api api) page)
  (let ((code (gethash 'code page))
        (text (gethash 'text page)))
    (if (eql code 200) ; that means HTTP_OK
        text
        nil)))

(defmacro def-req (request (&key url
                                 (method :get)
                                 version
                                 sub-url) docstring)
  (let ((area-url (first (split-sequence #\/ (string-downcase request))))
        (from-subs (mapcar #'string-downcase sub-url)))
    (alexandria:with-gensyms (base-url api-version url-version ready-url full-url)
      `(progn
         (defun ,request (&key (connection *connection*) parameters ,@sub-url)
           ,docstring
           (with-slots ((,base-url api-url)
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
               (url-read connection ,full-url :parameters parameters :method ,method :request-type :api))))
         (export ',request :odesk)))))

(defmacro def-gds (request (&key url
                                 (method :get)
                                 version
                                 sub-url) docstring)
  (let ((area-url (first (split-sequence #\/ (string-downcase request))))
        (from-subs (mapcar #'string-downcase sub-url)))
    (alexandria:with-gensyms (base-url api-version url-version ready-url full-url)
      `(progn
         (defun ,request (&key (connection *connection*) parameters ,@sub-url)
           ,docstring
           (with-slots ((,base-url gds-url)
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
               (url-read connection ,full-url :parameters parameters :method ,method :request-type :gds))))
         (export ',request :odesk)))))

; Example: (with-odesk (:connection con :public-key "PK" :secret-key "SK") (print con))
(defmacro with-odesk ((&key (connection '*connection*)
                            (format :json)
                            public-key
                            secret-key
                            api-token)
                      &body body)
  (let ((api-class 'odesk:api))
    `(progn
       (let ((,connection (make-instance ,api-class
                                         :data-format format
                                         :public-key ,public-key
                                         :secret-key ,secret-key
                                         :api-token ,api-token)))
         ,@body))))

; Example: (connect-odesk (:public-key "PK" :secret-key "SK"))
(defmacro connect-odesk ((&key (format :json)
                               public-key
                               secret-key
                               api-token))
  (let ((api-class 'odesk:api))
    `(progn
       (setf *connection* (make-instance ,api-class
                                         :data-format format
                                         :public-key ,public-key
                                         :secret-key ,secret-key
                                         :api-token ,api-token)))))