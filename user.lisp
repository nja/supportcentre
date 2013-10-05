(in-package #:supportcentre)

(defclass user (storable)
  ((name :initarg :name :accessor user-name)))

(defmethod serialize ((user user))
  (with-output-to-string (out)
    (prin1 (list :name (user-name user))
           out)))
