(in-package #:supportcentre)

(defclass area (storable linkable)
  ((name :initarg :name :accessor name-of)
   (owner :initarg :owner :accessor user-of)))

(defmethod serialize nconc ((area area))
  (list :name (name-of area)
        :owner (storage-id (user-of area))))

(defmethod storage-dependencies ((type (eql 'area)))
  '((user-of user)))

(defmethod issues-of ((area area) &key (from 0) (to -1))
  (storage-read-backrefs 'issue area :from from :to to))
