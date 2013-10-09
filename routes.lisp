(in-package #:supportcentre)

(restas:define-route issue-list ("")
  (list :title "Hello, World"
        :body (supportcentre.view:issue-list
               (list :issues
                     (redis:with-persistent-connection ()
                       (storage-read-set 'issue :all))))))

(restas:define-route issue ("/issue/:id")
  (let ((issue (redis:with-persistent-connection ()
                 (storage-read 'issue id))))
    (if issue
        (list :title (format nil "Issue #~a: ~a"
                             (storage-id issue)
                             (issue-subject issue))
              :body (supportcentre.view:issue
                     (list :issue issue)))
        hunchentoot:+HTTP-NOT-FOUND+)))

(restas:define-route create-issue ("/issue/new/")
  (list :title "Create new issue"
        :body (supportcentre.view:create-issue)))

(restas:define-route create-issue/post ("/issue/new/" :method :post)
  (:requirement #'(lambda () (hunchentoot:post-parameter "save")))
  (let ((id (redis:with-persistent-connection ()
              (storage-create (make-instance 'issue
                                             :subject
                                             (hunchentoot:post-parameter "subject")
                                             :creator
                                             (storage-read 'user 1))))))
    (restas:redirect (restas:genurl 'issue :id id))))

(restas:define-route login ("/login/")
  (list :title "Logged in"
        :body (list (your-session)
                    (as-text (hunchentoot:start-session))
                    (your-session))))

(restas:define-route logout ("/logout/")
  (list :title "Logged out"
        :body (list (your-session)
                    (let ((session hunchentoot:*session*))
                      (when session
                        (as-text (hunchentoot:remove-session session))))
                    (your-session))))
