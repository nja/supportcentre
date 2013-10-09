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
               (:file "storage" :depends-on ("package"))
               (:file "linkable" :depends-on ("storage"))
               (:file "user" :depends-on ("storage"))
               (:file "issue" :depends-on ("storage"))
               (:file "routes")))
