(in-package #:supportcentre)

(restas:define-route issue-list ("")
  (list :title "Hello, World"
        :body (supportcentre.view:issue-list
               (list :issues
                     (redis:with-persistent-connection ()
                       (get-all-issues))))))

(restas:define-route issue ("/issue/:id")
  (let ((issue (redis:with-persistent-connection ()
                 (get-issue id))))
    (if issue
        (list :title (format nil "Issue #~a: ~a"
                             (getf issue :id)
                             (getf issue :subject))
              :body (supportcentre.view:issue
                     (list :issue issue)))
        hunchentoot:+HTTP-NOT-FOUND+)))

(restas:define-route create-issue ("/issue/new/")
  (list :title "Create new issue"
        :body (supportcentre.view:create-issue)))

(restas:define-route create-issue/post ("/issue/new/" :method :post)
  (:requirement #'(lambda () (hunchentoot:post-parameter "save")))
  (let ((id (redis:with-persistent-connection ()
              (save-issue (list :subject (hunchentoot:post-parameter "subject"))))))
    (restas:redirect (restas:genurl 'issue :id id))))
