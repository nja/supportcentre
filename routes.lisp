(in-package #:supportcentre)

(restas:define-route issue-list ("")
  (list :title "Hello, World"
        :body (supportcentre.view:issue-list
               (list :issues
                     (redis:with-connection ()
                       (get-all-issues))))))
