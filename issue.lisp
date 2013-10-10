(in-package #:supportcentre)

(defgeneric issues-of (thing &key from to))

(defclass issue (storable linkable)
  ((subject :initarg :subject :accessor subject-of)
   (creator :initarg :creator :accessor user-of)
   (area :initarg :area :accessor area-of)))

(defmethod storage-dependencies ((type (eql 'issue)))
  '((user-of user)
    (area-of area)))

(defmethod serialize nconc ((issue issue))
  (list :subject (subject-of issue)
        :creator (storage-id (user-of issue))
        :area (storage-id (area-of issue))))

(defmethod notes-of ((issue issue) &key (from 0) (to -1))
  (storage-read-backrefs 'note issue :from from :to to))

(defmethod linkable-href ((issue issue))
  (restas:genurl 'issue
                 :area-id (storage-id (area-of issue))
                 :issue-id (storage-id issue)))
