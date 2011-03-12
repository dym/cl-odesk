; -*- coding: utf-8; mode: common-lisp; -*-

(in-package :odesk)

;; Auth info
; Example: (auth/get-frob *connection)
(def-req auth/get-frob
    (:url "keys/frobs"
          :method :post)
  "Get authentication frob.")

; Example: (auth/get-token *connection* :params '(("frob" . "enter_here_frob")))
(def-req auth/get-token
    (:url "keys/tokens"
          :method :post
          :params params)
  "Get authentication token.")

; Example: (auth/check-token *connection*)
(def-req auth/check-token
    (:url "keys/token"
          :method :get)
  "Return authenticated user associated with authorization token.")

; Example (auth/revoke-token *connection*)
(def-req auth/revoke-token
    (:url "keys/token"
          :method :delete)
  "Revoke given auth.")

;; Teams info
; Example: (team/get-teamrooms *connection*)
(def-req team/get-teamrooms
    (:url "teamrooms"
          :method :get
          :version 2)
  "Get all teamrooms accessible to the user.")

;; Profiles info
; Example: (profiles/get-providers *connection* :params '(("page" . "0;1") ("q" . "python")))
(def-req profiles/get-providers
    (:url "search/providers"
          :method :get
          :params params)
  "Search for workers.")

; Example: (profiles/get-jobs *connection* :params '(("page" . "0;1") ("q" . "python")))
(def-req profiles/get-jobs
    (:url "search/jobs"
          :method :get
          :params params)
  "Search for jobs.")