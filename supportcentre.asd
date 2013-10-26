;;;; supportcentre.asd

(defpackage #:supportcentre-system (:use #:asdf #:cl))
(in-package #:supportcentre-system)

(asdf:defsystem #:supportcentre
  :serial t
  :description "Support Issue Tracker"
  :author "Johan Andersson <nilsjohanandersson@gmail.com>"
  :license "Proprietary"
  :defsystem-depends-on (#:closure-template)
  :depends-on (#:restas
               #:closure-template
               #:cl-redis
               #:alexandria
               #:ironclad
               #:local-time)
  :in-order-to ((test-op (test-op #:supportcentre-test)))
  :components ((:module "templates"
                :components ((:closure-template "main")
                             (:closure-template "user")
                             (:closure-template "issue")
                             (:closure-template "login")
                             (:closure-template "area")))
               (:module "base"
                :pathname ""
                :serial t
                :components ((:file "package")
                             (:file "util")
                             (:file "generics")
                             (:file "storage")
                             (:file "linkable")
                             (:file "timed")
                             (:file "user")
                             (:file "area")
                             (:file "issue")
                             (:file "file")
                             (:file "note")
                             (:file "view")))
               (:module "routes"
                :components ((:file "routes-util")
                             (:file "posts")
                             (:file "views")
                             (:file "redirects")))))

(defsystem #:supportcentre-test
  :serial t
  :depends-on (#:supportcentre #:hu.dwim.stefil)
  :components ((:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :supportcentre-test))))
  (funcall (intern (symbol-name :test-all) (find-package :supportcentre-test))))
