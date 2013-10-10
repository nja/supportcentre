(in-package #:supportcentre)

(defclass issue (storable linkable)
  ((subject :initarg :subject :accessor subject-of)
   (creator :initarg :creator :accessor user-of)))

(defmethod storage-dependencies ((type (eql 'issue)))
  '((user-of user)))

(defmethod serialize ((issue issue))
  (prin1-to-string
   (list :subject (subject-of issue)
         :creator (storage-id (user-of issue)))))
