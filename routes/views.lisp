(in-package #:supportcentre)

(restas:define-route area-list ("")
  (with-storage
    (must-be-logged-in)
    (let ((*page-size* 20)
          (areas (read-all-areas))
          (page (get-page)))
      (list :title "Support Centre Areas"
            :areas (page-range areas page)
            :pages (make-page-links (pages areas) page 'area-list)
            :links (make-links
                    (list :href (restas:genurl 'user-list) :text "Users")
                    (get-user)
                    (login/out))))))

(restas:define-route area ("/area/:id")
  (:sift-variables (id 'integer))
  (with-storage
    (must-be-logged-in)
    (when-let (area (storage-read 'area id))
      (must-be-member area :reader)
      (list :title (format nil "Area #~d: ~a"
                           (storage-id area)
                           (name-of area))
            :area area
            :issues (issues-of area :page (get-page))
            :posterp (memberp area :poster)
            :links (make-links (home) (get-user) (login/out))
            :pages (make-page-links (pages-of 'issue area) (get-page)
                                    'area :id id)))))

(restas:define-route issue ("/area/:area-id/issue/:issue-id")
  (:sift-variables (area-id 'integer) (issue-id 'integer))
  (with-storage
    (must-be-logged-in)
    (when-let (issue (storage-read 'issue issue-id))
      (must-be-member (area-of issue) :reader)
      (when (equal area-id (storage-id (area-of issue)))
        (list :title (format nil "Issue #~d: ~a"
                             (storage-id issue)
                             (subject-of issue))
              :issue issue
              :notes (load-note-files
                      (markdown-notes
                       (notes-of issue :page (get-page))))
              :area (area-of issue)
              :posterp (memberp (area-of issue) :poster)
              :links (make-links (home) (area-of issue) (get-user) (login/out))
              :pages (make-page-links (pages-of 'note issue) (get-page)
                                      'issue :area-id area-id :issue-id issue-id))))))

(restas:define-route note ("/area/:area-id/issue/:issue-id/note/:note-id")
  (:sift-variables (area-id 'integer) (issue-id 'integer) (note-id 'integer))
  (with-storage
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
  (with-storage
    (must-be-logged-in)
    (when-let (user (storage-read 'user id))
      (let ((page (get-page)))
        (list :title (format nil "User ~@r: ~a" id (name-of user))
              :user user
              :issues (issues-of user :page page)
              :pages (make-page-links (pages-of 'issue user) page
                                      'user :id id)
              :links (make-links (home) (get-user) (login/out)))))))

(restas:define-route user-list ("/user/")
  (with-storage
    (must-be-logged-in)
    (let ((users (read-all-users))
          (page (get-page)))
     (list :title "User list"
           :users (page-range users page)
           :pages (make-page-links (pages users) page 'user-list)
           :links (make-links (home) (get-user) (login/out))))))

(restas:define-route register ("/register/")
  (list :title "Register an account"
        :username (get-parameter "username")
        :realname (get-parameter "realname")
        :message (get-parameter "message")
        :links (make-links (home) (get-user) (login/out))))

(restas:define-route file ("/file/:id/:name")
  (:sift-variables (id 'integer) (name 'string))
  (declare (ignore name))
  (with-storage
    (must-be-logged-in)
    (when-let (file (storage-read 'file id))
      (must-be-member (area-of file) :reader)
      (hunchentoot:handle-static-file (stored-path-of file)
                                      (mime-type-of file)))))

(restas:define-route login ("/login/")
  (list :title "Log in"
        :forward (get-parameter "forward")
        :links (make-links (list :href (restas:genurl 'register) :text "Register"))))
