(in-package #:cl-user)

(defpackage #:supportcentre-markdown
  (:use #:cl #:esrap #:3bmd-ext)
  (:export #:issue-link
           #:markdown))

(restas:define-module #:supportcentre
  (:use #:cl #:alexandria #:local-time)
  (:import-from #:hunchentoot #:post-parameter #:get-parameter)
  (:import-from #:url-rewrite #:url-encode)
  (:import-from #:supportcentre-markdown #:markdown))

(local-time:enable-read-macros)
