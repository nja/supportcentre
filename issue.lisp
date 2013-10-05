(in-package #:supportcentre)

(defclass issue (storable)
  ((subject :initarg :subject :accessor issue-subject)))

(defmethod serialize ((issue issue))
  (with-output-to-string (out)
    (prin1 (list :subject (issue-subject issue))
           out)))
