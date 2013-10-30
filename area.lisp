(in-package #:supportcentre)

(defclass area (storable linkable)
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

(defmethod storage-update :after ((area area))
  (red:set (lookup-key 'area :name (name-of area))
           (storage-id area)))

(defmethod issues-of ((area area) &rest keys)
  (apply #'storage-read-backrefs 'issue area keys))

(defun read-all-areas ()
  (sort (storage-read-set 'area 'area :all)
        #'string-lessp
        :key #'name-of))

(defun create-new-area (name owner)
  (unless (and (stringp name) (string< "" name))
    (return-from create-new-area
      (values nil "Name required")))
  (let ((area (make-instance 'area
                             :name name
                             :owner owner)))
    (if (red:setnx (lookup-key 'area :name (name-of area))
                   "pending")
        (values (storage-create area) "OK")
        (values nil "Area name not available."))))
