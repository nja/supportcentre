(in-package #:supportcentre)

(restas:define-route issue-list ("")
  (list :title "Hello, World"
        :issues (redis:with-persistent-connection ()
                  (storage-read-set 'issue :all))))

(restas:define-route issue ("/issue/:id")
  (let ((issue (redis:with-persistent-connection ()
                 (storage-read 'issue id))))
    (when issue
      (list :title (format nil "Issue #~a: ~a"
                           (storage-id issue)
                           (issue-subject issue))
            :issue issue))))

(restas:define-route user ("/user/:id")
  (let ((user (redis:with-persistent-connection ()
                (storage-read 'user id))))
    (list :title (format nil "User ~D: ~A"
                         (storage-id user)
                         (user-name user))
          :user user)))

(restas:define-route create-issue ("/issue/new/")
  (list :title "Create new issue"))

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

(restas:define-route register ("/register/")
  (list :title "Register an account"))

(restas:define-route register/post ("/register/" :method :post)
  (:requirement #'(lambda () (hunchentoot:post-parameter "register")))
  (let ((user (make-instance 'user
                             :name (hunchentoot:post-parameter "username")
                             :realname (hunchentoot:post-parameter "realname"))))
    (redis:with-persistent-connection ()
      (storage-create user))
    (list :title "Register"
          :body (format nil "Registered ~A (~A) #~D"
                        (user-name user)
                        (user-realname user)
                        (storage-id user)))))
