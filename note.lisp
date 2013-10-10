(in-package #:supportcentre)

(defgeneric notes-of (thing &key from to))

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

(defmethod linkable-href ((note note))
  (restas:genurl 'note
                 :area-id (storage-id (area-of note))
                 :issue-id (storage-id (issue-of note))
                 :note-id (storage-id note)))

(defmethod area-of ((note note))
  (area-of (issue-of note)))
