;;;; package.lisp

(restas:define-module #:supportcentre
  (:use #:cl #:alexandria #:local-time)
  (:import-from #:hunchentoot #:post-parameter))

(local-time:enable-read-macros)
