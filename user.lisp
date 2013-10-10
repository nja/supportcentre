(in-package #:supportcentre)

(defclass user (storable linkable)
  ((name :initarg :name :initform "" :accessor user-name)
   (realname :initarg :realname :initform "" :accessor user-realname)))

(defmethod serialize ((user user))
  (with-output-to-string (out)
    (prin1 (list :name (user-name user)
                 :realname (user-realname user))
           out)))

(defmethod storage-update :after ((user user))
  (red:set (lookup-key 'user 'name (user-name user))
           (storage-id user)))
