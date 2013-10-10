(in-package #:supportcentre)

(defclass note (storable)
  ((issue-id :initarg :issue-id :initform nil :accessor note-issue-id)
   (user-id :initarg :user-id :initform nil :accessor note-user-id)
   (text :initarg :text :initform "" :accessor note-text)))

(defmethod serialize ((note note))
  (prin1-to-string
   (list :issue-id (note-issue-id note)
         :user-id (note-user-id note)
         :text (note-text note))))

(defmethod storage-create :after ((note note))
  (redis:with-pipelining
    (red:rpush (make-key 'issue (note-issue-id note) 'notes)
               (storage-id note))
    (red:sadd (make-key 'user (note-user-id note) 'notes)
              (storage-id note))))

(defun issue-notes (issue &key (from 0) (to -1))
  (let ((ids (red:lrange (make-key 'issue (storage-id issue) 'notes) from to)))
    (storage-read-many 'note ids)))
