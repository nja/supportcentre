(in-package #:supportcentre)

(restas:define-route area-list ("")
  (list :title "Support Centre Areas"
        :areas (redis:with-persistent-connection ()
                 (storage-read-set 'area 'area :all))
        :links (list (list :href (restas:genurl 'user-list) :text "Users")
                     (list :href (restas:genurl 'register) :text "Register")
                     (list :href (restas:genurl 'login) :text "Login"))))

(restas:define-route area ("/area/:id")
  (:sift-variables (id 'integer))
  (redis:with-persistent-connection ()
    (must-be-logged-in)
    (when-let (area (storage-read 'area id))
      (list :title (format nil "Area #~d: ~a"
                           (storage-id area)
                           (name-of area))
            :area area
            :issues (issues-of area)
            :links (make-links (home))))))

(defun make-links (&rest linkables)
  (mapcar (lambda (thing)
            (if (listp thing)
                thing
                (multiple-value-bind (href text) (linkable-href thing)
                  (list :href href :text text))))
          linkables))

(defun home ()
  (list :href (restas:genurl 'area-list) :text "Home"))

(restas:define-route issue ("/area/:area-id/issue/:issue-id")
  (:sift-variables (area-id 'integer) (issue-id 'integer))
  (redis:with-persistent-connection ()
    (must-be-logged-in)
    (when-let (issue (storage-read 'issue issue-id))
      (when (equal area-id (storage-id (area-of issue)))
        (list :title (format nil "Issue #~d: ~a"
                             (storage-id issue)
                             (subject-of issue))
              :issue issue
              :notes (load-note-files (notes-of issue))
              :area (area-of issue)
              :links (make-links (home) (area-of issue)))))))

(restas:define-route note ("/area/:area-id/issue/:issue-id/note/:note-id")
  (:sift-variables (area-id 'integer) (issue-id 'integer) (note-id 'integer))
  (redis:with-persistent-connection ()
    (must-be-logged-in)
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
                :note note
                :links (make-links (home) area)))))))

(restas:define-route user ("/user/:id")
  (:sift-variables (id 'integer))
  (redis:with-persistent-connection ()
    (must-be-logged-in)
    (when-let (user (storage-read 'user id))
      (list :title (format nil "User ~@r: ~a"
                           (storage-id user)
                           (name-of user))
            :user user
            :issues (issues-of user)
            :links (make-links (home))))))

(restas:define-route user-list ("/user/")
  (redis:with-persistent-connection ()
    (must-be-logged-in)
    (list :title "User list"
          :users (redis:with-persistent-connection ()
                   (storage-read-set 'user 'user :all))
          :links (make-links (home)))))

(restas:define-route register ("/register/")
  (list :title "Register an account"))

(restas:define-route file ("/file/:id/:name")
  (:sift-variables (id 'integer) (name 'string))
  (declare (ignore name))
  (redis:with-persistent-connection ()
    (must-be-logged-in)
    (when-let (file (storage-read 'file id))
      (hunchentoot:handle-static-file (stored-path-of file)
                                      (mime-type-of file)))))

(defun must-be-logged-in ()
  (unless (get-user)
    (restas:redirect 'login :forward (hunchentoot:request-uri*))))
