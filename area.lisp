(in-package #:supportcentre)

(defclass area (storable linkable timed)
  ((name :initarg :name :accessor name-of)
   (owner :initarg :owner :accessor user-of)))

(defmethod serialize nconc ((area area))
  (list :name (name-of area)
        :owner (storage-id (user-of area))))

(defmethod linkable-href ((area area))
  (values
   (call-next-method)
   (name-of area)))

(defmethod storage-dependencies ((type (eql 'area)))
  '((user-of user)))

(defmethod issues-of ((area area) &rest keys)
  (apply #'storage-read-backrefs 'issue area keys))
