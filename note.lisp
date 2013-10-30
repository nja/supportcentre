(in-package #:supportcentre)

(defclass note (storable linkable)
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
  (values
   (restas:genurl 'note
                  :area-id (storage-id (area-of note))
                  :issue-id (storage-id (issue-of note))
                  :note-id (storage-id note))
   (format nil "~a: ~a" (user-of note) (creation-time-of note))))

(defmethod area-of ((note note))
  (area-of (issue-of note)))

(defmethod files-of ((note note) &rest keys)
  (apply #'storage-read-backrefs 'file note keys))

(defmethod storage-update :after ((note note))
  (storage-update (issue-of note)))

(defun load-note-files (notes)
  (mapc (lambda (n) (setf (slot-value n 'files) (files-of n)))
        notes))

(defun markdown-notes (notes)
  (mapc (lambda (n) (setf (text-of n) (markdown (text-of n))))
        notes))
