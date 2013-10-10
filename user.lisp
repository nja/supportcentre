(in-package #:supportcentre)

(defclass user (storable linkable)
  ((name :initarg :name :accessor user-name)
   (realname :initarg :realname :accessor user-realname)))

(defmethod serialize ((user user))
  (with-output-to-string (out)
    (prin1 (list :name (user-name user)
                 :realname (user-realname user))
           out)))
