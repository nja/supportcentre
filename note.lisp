(in-package #:supportcentre)

(defclass note (storable linkable)
  ((issue :initarg :issue :initform nil :accessor issue-of)
   (user :initarg :user :initform nil :accessor user-of)
   (text :initarg :text :initform "" :accessor text-of)))

(defmethod storage-dependencies ((type (eql 'note)))
  '((issue-of issue)
    (user-of user)))

(defmethod serialize ((note note))
  (prin1-to-string
   (list :issue (storage-id (issue-of note))
         :user (storage-id (user-of note))
         :text (text-of note))))

(defmethod storage-create :after ((note note))
  (redis:with-pipelining
    (dolist (type '(issue user))
      (red:rpush (make-key type (storage-id (slot-value note type)) 'notes)
                 (storage-id note)))))

(defmethod linkable-href ((note note))
  (restas:genurl 'issue-note
                 :issue-id (storage-id (issue-of note))
                 :note-id (storage-id note)))

(defun issue-notes (issue &key (from 0) (to -1))
  (let ((ids (red:lrange (make-key 'issue (storage-id issue) 'notes) from to)))
    (storage-read-many 'note ids)))
