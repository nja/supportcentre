(in-package #:supportcentre)

(defgeneric issues-of (thing &key start stop))

(defclass issue (storable linkable timed)
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

(defmethod notes-of ((issue issue) &key (start 0) (stop -1))
  (storage-read-backrefs 'note issue :start start :stop stop))

(defmethod linkable-href ((issue issue))
  (values
   (restas:genurl 'issue
                  :area-id (storage-id (area-of issue))
                  :issue-id (storage-id issue))
   (subject-of issue)))

(defmethod storage-update :after ((issue issue))
  (storage-update (area-of issue)))
