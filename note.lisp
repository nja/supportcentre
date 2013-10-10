(in-package #:supportcentre)

(defclass note (storable linkable)
  ((issue :initarg :issue :initform nil :accessor note-issue)
   (user :initarg :user :initform nil :accessor note-user)
   (text :initarg :text :initform "" :accessor note-text)))

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

(defun load-note-slots (notes)
  (let* ((issue-ids (mapcar #'note-issue notes))
         (user-ids (mapcar #'note-user notes))
         (issues (storage-read-many 'issue issue-ids))
         (users (storage-read-many 'user user-ids)))
    (mapc (lambda (note issue user)
            (setf (note-issue note) issue
                  (note-user note) user))
          notes
          issues
          users)))

(defmethod storage-read-many :around ((type (eql 'note)) ids)
  (load-note-slots (call-next-method)))

(defmethod storage-read :around ((type (eql 'note)) ids)
  (let ((note (call-next-method)))
    (when note (load-note-slots (list note)))
    note))
