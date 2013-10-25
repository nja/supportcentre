(in-package #:supportcentre)

(restas:define-route area-list ("")
  (redis:with-persistent-connection ()
    (must-be-logged-in)
    (list :title "Support Centre Areas"
          :areas (storage-read-set 'area 'area :all)
          :links (make-links
                  (list :href (restas:genurl 'user-list) :text "Users")
                  (get-user)
                  (login/out)))))

(restas:define-route area ("/area/:id")
  (:sift-variables (id 'integer))
  (redis:with-persistent-connection ()
    (must-be-logged-in)
    (when-let (area (storage-read 'area id))
      (must-be-member area :reader)
      (list :title (format nil "Area #~d: ~a"
                           (storage-id area)
                           (name-of area))
            :area area
            :issues (issues-of area)
            :posterp (memberp area :poster)
            :links (make-links (home) (get-user) (login/out))))))

(defun make-links (&rest linkables)
  (mapcar (lambda (thing)
            (if (listp thing)
                thing
                (multiple-value-bind (href text) (linkable-href thing)
                  (list :href href :text text))))
          linkables))

(defun home ()
  (list :href (restas:genurl 'area-list) :text "Home"))

(defun login/out ()
  (if (get-user)
      (list :href (restas:genurl 'logout) :text "Log out")
      (list :href (restas:genurl 'login) :text "Log in")))

(restas:define-route issue ("/area/:area-id/issue/:issue-id")
  (:sift-variables (area-id 'integer) (issue-id 'integer))
  (redis:with-persistent-connection ()
    (must-be-logged-in)
    (when-let (issue (storage-read 'issue issue-id))
      (must-be-member (area-of issue) :reader)
      (when (equal area-id (storage-id (area-of issue)))
        (list :title (format nil "Issue #~d: ~a"
                             (storage-id issue)
                             (subject-of issue))
              :issue issue
              :notes (load-note-files (notes-of issue))
              :area (area-of issue)
              :posterp (memberp (area-of issue) :poster)
              :links (make-links (home) (area-of issue) (get-user) (login/out)))))))

(restas:define-route note ("/area/:area-id/issue/:issue-id/note/:note-id")
  (:sift-variables (area-id 'integer) (issue-id 'integer) (note-id 'integer))
  (redis:with-persistent-connection ()
    (must-be-logged-in)
    (when-let (note (storage-read 'note note-id))
      (let ((area (area-of note))
            (issue (issue-of note)))
        (must-be-member area :reader)
        (when (and (equal area-id (storage-id area))
                   (equal issue-id (storage-id issue)))
          (list :title (format nil "Issue #~d: ~a"
                               issue-id
                               (subject-of issue))
                :issue issue
                :notes (notes-of issue)
                :note note
                :posterp (memberp area :poster)
                :links (make-links (home) area (get-user) (login/out))))))))

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
            :links (make-links (home) (get-user) (login/out))))))

(restas:define-route user-list ("/user/")
  (redis:with-persistent-connection ()
    (must-be-logged-in)
    (list :title "User list"
          :users (redis:with-persistent-connection ()
                   (storage-read-set 'user 'user :all))
          :links (make-links (home) (get-user) (login/out)))))

(restas:define-route register ("/register/")
  (list :title "Register an account"))

(restas:define-route file ("/file/:id/:name")
  (:sift-variables (id 'integer) (name 'string))
  (declare (ignore name))
  (redis:with-persistent-connection ()
    (must-be-logged-in)
    (when-let (file (storage-read 'file id))
      (must-be-member (area-of file) :reader)
      (hunchentoot:handle-static-file (stored-path-of file)
                                      (mime-type-of file)))))

(restas:define-route login ("/login/")
  (list :title "Log in"
        :forward (hunchentoot:get-parameter "forward")
        :links (make-links (list :href (restas:genurl 'register) :text "Register"))))

(defun must-be-logged-in ()
  (unless (get-user)
    (restas:redirect 'login :forward (hunchentoot:request-uri*))))

(defun memberp (thing set)
  (let ((member-ids (read-id-set (thing-set-key thing set)))
        (user-id (get-user-id)))
    (member user-id member-ids)))

(defun must-be-member (thing set)
  (unless (memberp thing set)
    (forbidden)))

(defun forbidden ()
  (setf (hunchentoot:return-code hunchentoot:*reply*)
        hunchentoot:+http-forbidden+)
  (hunchentoot:abort-request-handler))
