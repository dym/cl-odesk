; -*- coding: utf-8; mode: common-lisp; -*-

(in-package :odesk)

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