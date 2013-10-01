;;;; supportcentre.asd

(asdf:defsystem #:supportcentre
  :serial t
  :description "Support Issue Tracker"
  :author "Johan Andersson <nilsjohanandersson@gmail.com>"
  :license "Proprietary"
  :defsystem-depends-on (#:closure-template)
  :depends-on (#:restas #:closure-template #:cl-redis)
  :components ((:closure-template "templates/main")
               (:file "package")
               (:file "util" :depends-on ("package"))
               (:file "issue" :depends-on ("util"))
               (:file "routes")))
