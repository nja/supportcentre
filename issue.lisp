(in-package #:supportcentre)

(defclass issue (storable linkable)
  ((subject :initarg :subject :accessor issue-subject)
   (creator :initarg :creator :accessor issue-creator)))

(defmethod storage-dependencies ((type (eql 'issue)))
  '((issue-creator user)))

(defmethod serialize ((issue issue))
  (with-output-to-string (out)
    (prin1 (list :subject (issue-subject issue)
                 :creator (storage-id (issue-creator issue)))
           out)))
