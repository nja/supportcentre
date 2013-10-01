;;;; package.lisp

(restas:define-module #:supportcentre
  (:use #:cl)
  (:render-method #'supportcentre.view:main))
