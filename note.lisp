(in-package #:supportcentre)

(defclass note (storable linkable)
  ((issue :initarg :issue :initform nil :accessor note-issue)
   (user :initarg :user :initform nil :accessor note-user)
   (text :initarg :text :initform "" :accessor note-text)))

(defmethod storage-dependencies ((type (eql 'note)))
  '((note-issue issue)
    (note-user user)))

(defmethod serialize ((note note))
  (prin1-to-string
   (list :issue (storage-id (note-issue note))
         :user (storage-id (note-user note))
         :text (note-text note))))

(defmethod storage-create :after ((note note))
  (redis:with-pipelining
    (dolist (type '(issue user))
      (red:rpush (make-key type (storage-id (slot-value note type)) 'notes)
                 (storage-id note)))))

(defmethod linkable-href ((note note))
  (restas:genurl 'issue-note
                 :issue-id (storage-id (note-issue note))
                 :note-id (storage-id note)))

(defun issue-notes (issue &key (from 0) (to -1))
  (let ((ids (red:lrange (make-key 'issue (storage-id issue) 'notes) from to)))
    (storage-read-many 'note ids)))
