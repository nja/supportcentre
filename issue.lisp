(in-package #:supportcentre)

(defgeneric issues-of (thing &key from to))

(defclass issue (storable linkable)
  ((subject :initarg :subject :accessor subject-of)
   (creator :initarg :creator :accessor user-of)))

(defmethod storage-dependencies ((type (eql 'issue)))
  '((user-of user)))

(defmethod serialize ((issue issue))
  (prin1-to-string
   (list :subject (subject-of issue)
         :creator (storage-id (user-of issue)))))

(defmethod notes-of ((issue issue) &key (from 0) (to -1))
  (storage-read-many-from 'note 'issue (storage-id issue) :notes
                          :from from
                          :to to))
