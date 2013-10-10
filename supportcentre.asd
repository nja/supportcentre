;;;; supportcentre.asd

(asdf:defsystem #:supportcentre
  :serial t
  :description "Support Issue Tracker"
  :author "Johan Andersson <nilsjohanandersson@gmail.com>"
  :license "Proprietary"
  :defsystem-depends-on (#:closure-template)
  :depends-on (#:restas #:closure-template #:cl-redis #:alexandria #:ironclad)
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
                             (:file "storage")
                             (:file "linkable")
                             (:file "user")
                             (:file "area")
                             (:file "issue")
                             (:file "note")
                             (:file "view")))
               (:module "routes"
                :components ((:file "posts")
                             (:file "views")
                             (:file "redirects")))))

