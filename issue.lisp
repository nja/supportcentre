(in-package #:supportcentre)

(defclass issue (storable linkable)
  ((subject :initarg :subject :accessor issue-subject)
   (creator :initarg :creator :accessor issue-creator)))

(defmethod serialize ((issue issue))
  (with-output-to-string (out)
    (prin1 (list :subject (issue-subject issue)
                 :creator (storage-id (issue-creator issue)))
           out)))

(defun set-users (issues)
  (let* ((user-ids (mapcar #'issue-creator issues))
         (users (storage-read-many 'user user-ids)))
    (mapc (lambda (issue user)
            (setf (issue-creator issue) user))
          issues
          users)))

(defmethod storage-read-many :around ((type (eql 'issue)) ids)
  (set-users (call-next-method)))

(defmethod storage-read :around ((type (eql 'issue)) ids)
  (let ((user (call-next-method)))
    (when user (set-users (list user)))
    user))
