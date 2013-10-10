(in-package #:supportcentre)

(restas:define-route note/redirect ("/note/:id")
  (:sift-variables (id 'integer))
  (when-let (note (redis:with-persistent-connection ()
                    (storage-read 'note id)))
    (restas:redirect 'note
                     :area-id (storage-id (area-of note))
                     :issue-id (storage-id (issue-of note))
                     :note-id (storage-id note))))
