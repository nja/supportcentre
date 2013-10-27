(in-package #:supportcentre)

(restas:define-route note/redirect ("/note/:id")
  (:sift-variables (id 'integer))
  (with-storage
    (must-be-logged-in)
    (when-let (note (storage-read 'note id))
      (hunchentoot:redirect (linkable-href note)))))

(restas:define-route issue/redirect ("/issue/:id")
  (:sift-variables (id 'integer))
  (with-storage
    (must-be-logged-in)
    (when-let (issue (storage-read 'issue id))
      (hunchentoot:redirect (linkable-href issue)))))
