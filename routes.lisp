(in-package #:supportcentre)

(restas:define-route issue-list ("")
  (list :title "Hello, World"
        :issues (redis:with-persistent-connection ()
                  (storage-read-set 'issue :all))))

(restas:define-route issue ("/issue/:id")
  (:sift-variables (id 'integer))
  (when-let (issue (redis:with-persistent-connection ()
                     (storage-read 'issue id)))
    (list :title (format nil "Issue #~a: ~a"
                         (storage-id issue)
                         (issue-subject issue))
          :issue issue)))

(restas:define-route user ("/user/:id")
  (:sift-variables (id 'integer))
  (when-let (user (redis:with-persistent-connection ()
                    (storage-read 'user id)))
    (list :title (format nil "User ~D: ~A"
                         (storage-id user)
                         (user-name user))
          :user user)))

(restas:define-route create-issue ("/issue/new/")
  (list :title "Create new issue"))

(restas:define-route create-issue/post ("/issue/new/" :method :post)
  (:requirement #'(lambda () (post-parameter "save")))
  (let ((id (redis:with-persistent-connection ()
              (storage-create (make-instance 'issue
                                             :subject
                                             (post-parameter "subject")
                                             :creator
                                             (storage-read 'user 1))))))
    (restas:redirect (restas:genurl 'issue :id id))))

(restas:define-route login ("/login/")
  (list :title "Log in"))

(restas:define-route login/post ("/login/" :method :post)
  (:requirement #'(lambda () (post-parameter "login")))
  (let ((user (redis:with-persistent-connection ()
                (storage-lookup 'user 'name (post-parameter "username")))))
    (when (correct-password-p user (post-parameter "password"))
      (set-user user)
      (restas:redirect (restas:genurl 'user :id (storage-id user))))
    (restas:redirect (restas:genurl 'login))))

(restas:define-route logout ("/logout/")
  (set-user nil)
  (restas:redirect (restas:genurl 'login)))

(restas:define-route register ("/register/")
  (list :title "Register an account"))

(restas:define-route register/post ("/register/" :method :post)
  (:requirement #'(lambda () (post-parameter "register")))
  (let ((user (make-instance 'user
                             :name (post-parameter "username")
                             :realname (post-parameter "realname"))))
    (redis:with-persistent-connection ()
      (storage-create user))
    (list :title "Register"
          :body (format nil "Registered ~A (~A) #~D"
                        (user-name user)
                        (user-realname user)
                        (storage-id user)))))
