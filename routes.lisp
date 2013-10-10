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
          :issue issue
          :notes (redis:with-persistent-connection ()
                   (issue-notes issue)))))

(restas:define-route note ("/note/:id")
  (:sift-variables (id 'integer))
  (redis:with-persistent-connection ()
    (when-let (note (storage-read 'note id))
      (restas:redirect 'issue-note
                       :issue-id (storage-id (note-issue note))
                       :note-id id))))

(restas:define-route issue-note ("/issue/:issue-id/note/:note-id")
  (:sift-variables (id 'integer))
  (redis:with-persistent-connection ()
    (when-let (note (storage-read 'note note-id))
      (let ((issue (note-issue note)))
        (when (equal issue-id (storage-id (note-issue note)))
          (list :title (format nil "Issue #~a: ~a"
                               issue-id
                               (issue-subject issue))
                :issue issue
                :notes (issue-notes issue)
                :note note))))))

(restas:define-route note/post ("/issue/:id/note/" :method :post)
  (:sift-variables (id 'integer))
  (:requirement #'(lambda () (post-parameter "save")))
  (redis:with-persistent-connection ()
    (let* ((issue (storage-read 'issue id))
           (note (make-instance 'note
                                :user (get-user)
                                :issue issue
                                :text (post-parameter "text"))))
      (storage-create note)
      (restas:redirect 'issue :id (storage-id issue)))))

(restas:define-route user ("/user/:id")
  (:sift-variables (id 'integer))
  (when-let (user (redis:with-persistent-connection ()
                    (storage-read 'user id)))
    (list :title (format nil "User ~D: ~A"
                         (storage-id user)
                         (user-name user))
          :user user)))

(restas:define-route issue/post ("/issue/" :method :post)
  (:requirement #'(lambda () (post-parameter "save")))
  (redis:with-persistent-connection ()
    (storage-create (make-instance 'issue
                                   :subject
                                   (post-parameter "subject")
                                   :creator
                                   (get-user))))
  (restas:redirect 'issue-list))

(restas:define-route login ("/login/")
  (list :title "Log in"))

(restas:define-route login/post ("/login/" :method :post)
  (:requirement #'(lambda () (post-parameter "login")))
  (let ((user (redis:with-persistent-connection ()
                (storage-lookup 'user 'name (post-parameter "username")))))
    (when (correct-password-p user (post-parameter "password"))
      (set-user user)
      (restas:redirect 'user :id (storage-id user)))
    (restas:redirect 'login)))

(restas:define-route logout ("/logout/")
  (set-user nil)
  (restas:redirect 'login))

(restas:define-route register ("/register/")
  (list :title "Register an account"))

(restas:define-route register/post ("/register/" :method :post)
  (:requirement #'(lambda () (post-parameter "register")))
  (let ((user (make-instance 'user
                             :name (post-parameter "username")
                             :realname (post-parameter "realname"))))
    (set-password user (post-parameter "password"))
    (redis:with-persistent-connection ()
      (storage-create user))
    (restas:redirect 'user :id (storage-id user))))
