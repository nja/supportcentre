(in-package #:supportcentre)

(restas:define-route issue/post ("/area/:area-id/issue/" :method :post)
  (:sift-variables (area-id 'integer))
  (:requirement #'(lambda () (post-parameter "save")))
  (with-storage
    (must-be-logged-in)
    (when-let (area (storage-read 'area area-id))
      (must-be-member area :poster)
      (storage-create (make-instance 'issue
                                     :area area
                                     :subject (post-parameter "subject")
                                     :creator (get-user)))))
  (restas:redirect 'area :id area-id))

(restas:define-route note/post ("/area/:area-id/issue/:issue-id/note/" :method :post)
  (:sift-variables (area-id 'integer) (issue-id 'integer))
  (:requirement #'(lambda () (post-parameter "save")))
  (with-storage
    (must-be-logged-in)
    (let ((issue (storage-read 'issue issue-id)))
      (when (equal area-id (storage-id (area-of issue)))
        (must-be-member (area-of issue) :poster)
        (let ((note (make-instance 'note
                                   :user (get-user)
                                   :issue issue
                                   :text (post-parameter "text"))))
          (storage-create note)
          (when-let (file (handle-upload (post-parameter "file")))
            (setf (note-of file) note)
            (storage-create file))
          (restas:redirect 'issue :area-id area-id :issue-id (storage-id issue)))))))

(restas:define-route login/post ("/login/" :method :post)
  (:requirement #'(lambda () (post-parameter "login")))
  (let ((user (with-storage
                (storage-lookup 'user :name (post-parameter "username")))))
    (when (correct-password-p user (post-parameter "password"))
      (set-user user))
    (when-let (forward (post-parameter "forward"))
      (hunchentoot:redirect forward))
    (restas:redirect 'area-list)))

(restas:define-route register/post ("/register/" :method :post)
  (:requirement #'(lambda () (post-parameter "register")))
  (let ((user (make-instance 'user
                             :name (post-parameter "username")
                             :realname (post-parameter "realname"))))
    (set-password user (post-parameter "password"))
    (with-storage
      (storage-create user))
    (restas:redirect 'user :id (storage-id user))))

(restas:define-route logout ("/logout/")
  (set-user nil)
  (restas:redirect 'login))
