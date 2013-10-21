(in-package #:supportcentre)

(restas:define-route area ("/area/:id")
  (:sift-variables (id 'integer))
  (redis:with-persistent-connection ()
    (when-let (area (storage-read 'area id))
      (list :title (format nil "Area #~d: ~a"
                           (storage-id area)
                           (name-of area))
            :area area
            :issues (issues-of area)
            :home (restas:genurl 'area-list)))))

(restas:define-route area-list ("")
  (list :title "Hello, World"
        :areas (redis:with-persistent-connection ()
                  (storage-read-set 'area :all))))

(restas:define-route issue ("/area/:area-id/issue/:issue-id")
  (:sift-variables (area-id 'integer) (issue-id 'integer))
  (when-let (issue (redis:with-persistent-connection ()
                     (storage-read 'issue issue-id)))
    (when (equal area-id (storage-id (area-of issue)))
      (list :title (format nil "Issue #~d: ~a"
                           (storage-id issue)
                           (subject-of issue))
            :issue issue
            :notes (redis:with-persistent-connection ()
                     (notes-of issue))
            :home (restas:genurl 'area-list)
            :area (area-of issue)))))

(restas:define-route note ("/area/:area-id/issue/:issue-id/note/:note-id")
  (:sift-variables (area-id 'integer) (issue-id 'integer) (note-id 'integer))
  (redis:with-persistent-connection ()
    (when-let (note (storage-read 'note note-id))
      (let ((area (area-of note))
            (issue (issue-of note)))
        (when (and (equal area-id (storage-id area))
                   (equal issue-id (storage-id issue)))
          (list :title (format nil "Issue #~d: ~a"
                               issue-id
                               (subject-of issue))
                :issue issue
                :notes (notes-of issue)
                :note note))))))

(restas:define-route user ("/user/:id")
  (:sift-variables (id 'integer))
  (redis:with-persistent-connection ()
    (when-let (user (storage-read 'user id))
      (list :title (format nil "User ~@r: ~a"
                           (storage-id user)
                           (name-of user))
            :user user
            :issues (issues-of user)))))

(restas:define-route register ("/register/")
  (list :title "Register an account"))

(defun must-be-logged-in ()
  (unless (get-user)
    (restas:redirect 'login)))
