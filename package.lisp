(in-package #:cl-user)

(restas:define-module #:supportcentre
  (:use #:cl #:alexandria #:local-time)
  (:import-from #:hunchentoot #:post-parameter)
  (:import-from #:url-rewrite #:url-encode))

(local-time:enable-read-macros)
