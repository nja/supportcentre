(in-package #:supportcentre)

(defgeneric notes-of (thing &key start stop))

(defclass note (storable linkable timed)
  ((issue :initarg :issue :initform nil :accessor issue-of)
   (user :initarg :user :initform nil :accessor user-of)
   (text :initarg :text :initform "" :accessor text-of)
   (files :initform nil)))

(defmethod storage-dependencies ((type (eql 'note)))
  '((issue-of issue)
    (user-of user)))

(defmethod serialize nconc ((note note))
  (list :issue (storage-id (issue-of note))
        :user (storage-id (user-of note))
        :text (text-of note)))

(defmethod linkable-href ((note note))
  (restas:genurl 'note
                 :area-id (storage-id (area-of note))
                 :issue-id (storage-id (issue-of note))
                 :note-id (storage-id note)))

(defmethod area-of ((note note))
  (area-of (issue-of note)))

(defmethod files-of ((note note) &key (start 0) (stop -1))
  (storage-read-backrefs 'file note :start start :stop stop))

(defun load-note-files (notes)
  (mapc (lambda (n) (setf (slot-value n 'files) (files-of n)))
        notes))
