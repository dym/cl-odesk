; -*- coding: utf-8; mode: common-lisp; -*-

(in-package :odesk)

;;;;;;;;;;;;;;;
;; Auth info
;;;
; Example: (auth/get-frob :connection *connection*)
(def-req auth/get-frob
    (:url "keys/frobs"
          :method :post)
  "Get authentication frob.")

; Example: (auth/get-token :connection *connection* :parameters '(("frob" . "enter_here_frob")))
(def-req auth/get-token
    (:url "keys/tokens"
          :method :post)
  "Get authentication token.")

; Example: (auth/check-token :connection *connection*)
(def-req auth/check-token
    (:url "keys/token"
          :method :get)
  "Return authenticated user associated with authorization token.")

; Example (auth/revoke-token :connection *connection*)
(def-req auth/revoke-token
    (:url "keys/token"
          :method :delete)
  "Revoke given auth.")

;;;;;;;;;;;;;;;
;; Messages Center
;;;
; Example: (mc/get-trays :connection *connection*)
(def-req mc/get-trays
    (:url "trays"
          :method :get)
  "Retrieve a list of all active trays and a message count for each.")

; Example: (mc/list-tray :connection *connection* :tray "inbox" :username "someuser")
(def-req mc/list-tray
    (:url "trays/{username}/{tray}"
          :sub-url (username tray)
          :method :get)
  "Retrive tray contents.")

;;;;;;;;;;;;;;;
;; Teams info
;;;
; Example: (team/get-teamrooms :connection *connection*)
(def-req team/get-teamrooms
    (:url "teamrooms"
          :method :get
          :version 2)
  "Get all teamrooms accessible to the user.")

;;;;;;;;;;;;;;;
;; Profiles info
;;;
; Example: (profiles/get-provider :connection *connection* :provider "~~abcdefghijklmno")
;          :provider is profile_key, which you can get by (hr/get-user :username "me") or by using search
(def-req profiles/get-provider
    (:url "providers/{provider}"
          :sub-url (provider)
          :method :get)
  "Return detailed profile information about provider")

; Example: (profiles/get-providers :connection *connection* :parameters '(("page" . "0;1") ("q" . "python")))
(def-req profiles/get-providers
    (:url "search/providers"
          :method :get)
  "Search for workers.")

; Example: (profiles/get-jobs :connection *connection* :parameters '(("page" . "0;1") ("q" . "python")))
(def-req profiles/get-jobs
    (:url "search/jobs"
          :method :get)
  "Search for jobs.")

;;;;;;;;;;;;;;;
;; HR info
;;;
; Example: (hr/get-user :connection *connection*)
(def-req hr/get-user
    (:url "users/me"
          :version 2
          :method :get)
  "Return info about current authenticated user")
